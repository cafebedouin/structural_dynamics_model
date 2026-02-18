% ============================================================================
% CONSTRAINT STORY: openai_codex_app_constraint
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_openai_codex_app_constraint, []).

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
 * * constraint_id: openai_codex_app_constraint
 * human_readable: Algorithmic Dependency
 * domain: technological
 * * SUMMARY:
 * The OpenAI Codex app enables AI agent development, but its control by a single entity (OpenAI) creates a dependency constraint. Developers become reliant on the app's continued availability, pricing, and feature set, potentially limiting their autonomy and innovation.
 * * KEY AGENTS:
 * - Developer: Subject (Powerless)
 * - OpenAI: Beneficiary (Institutional)
 * - Independent AI Ethics Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(openai_codex_app_constraint, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(openai_codex_app_constraint, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(openai_codex_app_constraint, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(openai_codex_app_constraint, extractiveness, 0.55).
narrative_ontology:constraint_metric(openai_codex_app_constraint, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(openai_codex_app_constraint, theater_ratio, 0.20).

% Constraint classification claim
narrative_ontology:constraint_claim(openai_codex_app_constraint, tangled_rope).
narrative_ontology:human_readable(openai_codex_app_constraint, "Algorithmic Dependency").
narrative_ontology:topic_domain(openai_codex_app_constraint, "technological").

% Binary flags
% narrative_ontology:has_sunset_clause(openai_codex_app_constraint).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(openai_codex_app_constraint). % Required for Tangled Rope

narrative_ontology:constraint_beneficiary(openai_codex_app_constraint, openai).
narrative_ontology:constraint_victim(openai_codex_app_constraint, developers).

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
constraint_indexing:constraint_classification(openai_codex_app_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(openai_codex_app_constraint, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(openai_codex_app_constraint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openai_codex_app_constraint_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(openai_codex_app_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openai_codex_app_constraint, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(openai_codex_app_constraint, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(openai_codex_app_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * I assigned a base extractiveness of 0.55 and suppression of 0.70 because developers are heavily reliant on Codex, limiting their options. A powerless developer perceives this as a Snare. OpenAI, as an institutional actor, views it as a Rope (essential service). An analytical observer recognizes the dual nature: OpenAI offers a valuable service, but the dependency creates asymmetric extraction. The perspectival gap arises from the unequal distribution of power and exit options. Developers are locked in, while OpenAI has more flexibility.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling this as pure extraction (Snare) because OpenAI provides a genuine coordination function (AI agent development platform). The key is that it also extracts value through platform lock-in and control. If OpenAI ceases active support of Codex or it is functionally replaced, the classification would shift toward Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_openai_codex_app_constraint,
    'Will open-source alternatives sufficiently mitigate developer lock-in?',
    'Monitor the adoption rate and capabilities of competing open-source AI agent development tools.',
    'If True: Dependency becomes less of a Snare. If False: Dependency remains a significant constraint.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openai_codex_app_constraint, 0, 10).

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
narrative_ontology:measurement(openai_codex_app_constraint_tr_t0, openai_codex_app_constraint, theater_ratio, 0, 0.10).
narrative_ontology:measurement(openai_codex_app_constraint_tr_t5, openai_codex_app_constraint, theater_ratio, 5, 0.20).
narrative_ontology:measurement(openai_codex_app_constraint_tr_t10, openai_codex_app_constraint, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(openai_codex_app_constraint_ex_t0, openai_codex_app_constraint, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(openai_codex_app_constraint_ex_t5, openai_codex_app_constraint, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(openai_codex_app_constraint_ex_t10, openai_codex_app_constraint, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */