% ============================================================================
% CONSTRAINT STORY: meta_nda
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_meta_nda, []).

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
 * * constraint_id: meta_nda
 * human_readable: Meta's Non-Disclosure Agreements for Undercover Testers
 * domain: economic
 * * SUMMARY:
 * Meta uses Non-Disclosure Agreements (NDAs) with its undercover testers in simulated online environments ("Simulated Terrorist Attack," "Simulated School Shooting") to prevent them from disclosing information about potential platform flaws or manipulative tactics being tested. These NDAs are being challenged in court by the state of New Mexico, who claim they unfairly restrict the testers' ability to speak out about harmful practices.
 * * KEY AGENTS:
 * - Testers: Subject (Powerless)
 * - Meta: Beneficiary (Institutional)
 * - New Mexico Attorney General: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(meta_nda, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(meta_nda, 0.8).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(meta_nda, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(meta_nda, extractiveness, 0.6).
narrative_ontology:constraint_metric(meta_nda, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(meta_nda, theater_ratio, 0.2).

% Constraint classification claim
narrative_ontology:constraint_claim(meta_nda, tangled_rope).
narrative_ontology:human_readable(meta_nda, "Meta's Non-Disclosure Agreements for Undercover Testers").

% Binary flags
% narrative_ontology:has_sunset_clause(meta_nda).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(meta_nda). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(meta_nda, meta).
narrative_ontology:constraint_victim(meta_nda, testers).

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
constraint_indexing:constraint_classification(meta_nda, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(meta_nda, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(meta_nda, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(meta_nda_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(meta_nda, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(meta_nda, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(meta_nda, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(meta_nda_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The NDAs are viewed as a Snare by the testers, who are effectively silenced about potential harms on the platform. Meta sees them as a necessary means (Rope) to protect their research and development process, but also leverages them to extract information and prevent reputational damage. The analytical observer classifies this as a tangled rope because it blends legitimate coordination (platform improvement) with the potential for asymmetric extraction (suppressing criticism of harmful practices).  The perspectival gap arises because Meta focuses on the coordination benefits, while the testers experience the restrictive suppression directly.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling as pure extraction because the NDAs *do* serve a legitimate purpose in protecting Meta's research. However, the high suppression score and the potential for abuse justify the "tangled" classification, preventing the system from falsely identifying a pure coordination mechanism. Without Tangled Rope, this would either become a simple Snare, ignoring the coordination function entirely, or a Rope, ignoring the suppression.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_meta_nda,
    'To what extent are Meta's research practices genuinely improved by the NDA, versus merely shielded from scrutiny?',
    'Independent audits of Meta's research methodology and a comparison with companies using alternative practices.',
    'If NDA is primarily for improvement: Classify as Rope. If primarily for shielding: Classify as Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(meta_nda, 0, 10).

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
narrative_ontology:measurement(meta_nda_tr_t0, meta_nda, theater_ratio, 0, 0.1).
narrative_ontology:measurement(meta_nda_tr_t5, meta_nda, theater_ratio, 5, 0.2).
narrative_ontology:measurement(meta_nda_tr_t10, meta_nda, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(meta_nda_ex_t0, meta_nda, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(meta_nda_ex_t5, meta_nda, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(meta_nda_ex_t10, meta_nda, base_extractiveness, 10, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */