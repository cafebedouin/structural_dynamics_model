% ============================================================================
% CONSTRAINT STORY: guinea_worm_eradication
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_guinea_worm_eradication, []).

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
 * * constraint_id: guinea_worm_eradication
 * * human_readable: Global Guinea Worm Eradication Program
 * * domain: social
 * * SUMMARY:
 * The Global Guinea Worm Eradication Program, led by The Carter Center, aims to eradicate Guinea worm disease through providing safe drinking water sources and health education, reducing human suffering in endemic regions. The program serves as a coordination mechanism, but also requires some level of enforcement by restricting access to unsafe water sources.  The program is nearing completion but remains an active effort.
 * * KEY AGENTS:
 * - Infected Individuals: Subject (Powerless)
 * - The Carter Center: Beneficiary (Institutional)
 * - Analytical Observer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(guinea_worm_eradication, 0.25). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(guinea_worm_eradication, 0.30).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(guinea_worm_eradication, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(guinea_worm_eradication, extractiveness, 0.25).
narrative_ontology:constraint_metric(guinea_worm_eradication, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(guinea_worm_eradication, theater_ratio, 0.10).

% Binary flags
% narrative_ontology:has_sunset_clause(guinea_worm_eradication).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(guinea_worm_eradication). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(guinea_worm_eradication, endemic_communities).
narrative_ontology:constraint_victim(guinea_worm_eradication, endemic_communities).

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
constraint_indexing:constraint_classification(guinea_worm_eradication, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(guinea_worm_eradication, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(guinea_worm_eradication, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(guinea_worm_eradication_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(guinea_worm_eradication, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(guinea_worm_eradication, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(guinea_worm_eradication, ExtMetricName, E),
    E > 0.05. % Ensures extraction is occurring

:- end_tests(guinea_worm_eradication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The program is viewed as a Snare by infected individuals because they face restricted access to water sources which feels coercive, even though it's intended to prevent infection. The Carter Center sees it as a Rope, essential for coordinating global health efforts.  The analytical observer classifies it as a Tangled Rope due to the combination of a coordination function (eradication program) and an asymmetric extraction (restriction of water source choice). The perspectival gap stems from the power dynamics; the individual lacks the power to choose safe alternatives and views the restrictions negatively, while the institution focuses on the aggregate benefit.  The base extractiveness score is relatively low (0.25) because the restrictions are intended to be temporary and facilitate a long-term positive outcome. Suppression is scored at 0.30 because the enforcement involves restricting access, even if alternatives are provided by the program.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the program as pure extraction (Snare). Without recognizing the coordination aspect (disease eradication), the system might focus solely on the restrictions and label it negatively. The has_coordination_function predicate, derived from constraint_beneficiary, is crucial for the Tangled Rope determination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_guinea_worm,
    'Will the final stages of eradication be sustainable without continued external intervention?',
    'Long-term monitoring of water source maintenance and local capacity building.',
    'If True: Full eradication achieved. If False: Disease resurgence.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(guinea_worm_eradication, 0, 10).

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
narrative_ontology:measurement(guinea_worm_eradication_tr_t0, guinea_worm_eradication, theater_ratio, 0, 0.15).
narrative_ontology:measurement(guinea_worm_eradication_tr_t5, guinea_worm_eradication, theater_ratio, 5, 0.12).
narrative_ontology:measurement(guinea_worm_eradication_tr_t10, guinea_worm_eradication, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(guinea_worm_eradication_ex_t0, guinea_worm_eradication, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(guinea_worm_eradication_ex_t5, guinea_worm_eradication, base_extractiveness, 5, 0.30).
narrative_ontology:measurement(guinea_worm_eradication_ex_t10, guinea_worm_eradication, base_extractiveness, 10, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */