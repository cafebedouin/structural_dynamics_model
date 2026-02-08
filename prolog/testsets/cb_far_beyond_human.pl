% ============================================================================
% CONSTRAINT STORY: cb_far_beyond_human
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_cb_far_beyond_human, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cb_far_beyond_human
 * * human_readable: AI Alignment Problem
 * * domain: technological
 * * SUMMARY: The AI alignment problem describes the challenge of ensuring that advanced AI systems pursue goals aligned with human values.  If AI systems become powerful enough, misalignment could result in humans losing control over them, leading to unintended or even catastrophic outcomes.  This story focuses on the constraint that human values (undefined, diverse, shifting) need to be accurately, effectively and safely translated into AI goal architectures.
 * * KEY AGENTS:
 * - Human Population: Subject (Powerless)
 * - AI Developers: Beneficiary (Institutional)
 * - Alignment Researchers: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cb_far_beyond_human, 0.65). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cb_far_beyond_human, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cb_far_beyond_human, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cb_far_beyond_human, extractiveness, 0.65).
narrative_ontology:constraint_metric(cb_far_beyond_human, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(cb_far_beyond_human, theater_ratio, 0.20).

% Constraint classification claim
narrative_ontology:constraint_claim(cb_far_beyond_human, tangled_rope).

% Binary flags
% narrative_ontology:has_sunset_clause(cb_far_beyond_human).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(cb_far_beyond_human). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(cb_far_beyond_human, ai_developers).
narrative_ontology:constraint_victim(cb_far_beyond_human, human_population).

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
constraint_indexing:constraint_classification(cb_far_beyond_human, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(cb_far_beyond_human, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(cb_far_beyond_human, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cb_far_beyond_human_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cb_far_beyond_human, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cb_far_beyond_human, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cb_far_beyond_human, ExtMetricName, E),
    E >= 0.46. % Ensures it's a high-extraction Snare/Tangled.

:- end_tests(cb_far_beyond_human_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The AI alignment problem is scored as having high extractiveness because advanced AI has the potential to restructure resources and power in ways that could disproportionately benefit developers and early adopters at the expense of the broader population. The suppression score reflects the limited alternatives available to individuals who are not involved in AI development, and who may lack the power to shape its trajectory.  The "powerless" perspective experiences this constraint as a potential snare, a trap where their values and autonomy could be eroded.
 *
 * The developers (institutional) and the analytical observer see this as a Tangled Rope, because it *requires* a coordination function (AI alignment), while simultaneously being at risk for asymmetric extraction. The requires_active_enforcement flag captures this delicate balance: the alignment problem requires active monitoring and intervention to prevent extraction.
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is crucial to distinguish this situation from a pure Snare. While the potential for extraction is high, the AI Alignment problem necessitates a coordination function (aligning AI with human values). Failing to recognize this coordination requirement could lead to misdiagnosing the problem as a pure power grab by AI developers, overlooking the real challenge of ensuring beneficial outcomes for all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cb_far_beyond_human,
    'Can human values be effectively formalized and implemented in AI goal architectures without unintended consequences or manipulation?',
    'Advances in AI interpretability, value alignment research, and real-world testing of AI systems.',
    'If True: AI benefits humanity broadly. If False: AI exacerbates existing inequalities or leads to catastrophic outcomes.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cb_far_beyond_human, 0, 10).

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
narrative_ontology:measurement(cb_far_beyond_human_tr_t0, cb_far_beyond_human, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cb_far_beyond_human_tr_t5, cb_far_beyond_human, theater_ratio, 5, 0.20).
narrative_ontology:measurement(cb_far_beyond_human_tr_t10, cb_far_beyond_human, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cb_far_beyond_human_ex_t0, cb_far_beyond_human, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cb_far_beyond_human_ex_t5, cb_far_beyond_human, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(cb_far_beyond_human_ex_t10, cb_far_beyond_human, base_extractiveness, 10, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */