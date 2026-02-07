% ============================================================================
% CONSTRAINT STORY: openclaw_regulation
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_openclaw_regulation, []).

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
 * * constraint_id: openclaw_regulation
 * human_readable: Regulation of Autonomous AI Assistant OpenClaw
 * domain: technological
 * * SUMMARY:
 * The emergence of OpenClaw, a viral AI assistant with autonomous capabilities, necessitates regulatory frameworks to address potential misuse and security risks. These regulations aim to balance innovation with safeguards against unintended consequences and malicious exploitation.
 * * KEY AGENTS:
 * - Users: Subject (Powerless)
 * - Regulators/Developers: Beneficiary (Institutional)
 * - Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(openclaw_regulation, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(openclaw_regulation, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(openclaw_regulation, 0.30).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(openclaw_regulation, extractiveness, 0.55).
narrative_ontology:constraint_metric(openclaw_regulation, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(openclaw_regulation, theater_ratio, 0.30).

% Binary flags
% narrative_ontology:has_sunset_clause(openclaw_regulation).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(openclaw_regulation). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(openclaw_regulation, developers_regulators).
narrative_ontology:constraint_victim(openclaw_regulation, users).

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
constraint_indexing:constraint_classification(openclaw_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(openclaw_regulation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(openclaw_regulation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
/*constraint_indexing:constraint_classification(openclaw_regulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))) :-
    narrative_ontology:has_sunset_clause(openclaw_regulation).*/

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
/*constraint_indexing:constraint_classification(openclaw_regulation, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(openclaw_regulation, TR), TR > 0.70.*/

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(openclaw_regulation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(openclaw_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(openclaw_regulation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(openclaw_regulation, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(openclaw_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The OpenClaw regulation is classified as a Tangled Rope because it involves both coordination and asymmetric extraction. Regulators and developers benefit from the centralized control and potential profit (coordination), while users face potential privacy violations and manipulation (asymmetric extraction). The suppression score is high due to the potential for limited user choice and the enforcement of regulatory measures. The perspectival gap arises because users, being powerless, feel trapped by the regulation, while regulators view it as a necessary mechanism for societal benefit.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the regulation as pure extraction (Snare) by recognizing the coordination function provided to developers and regulators in maintaining the AI system and setting standards. This distinguishes it from purely exploitative systems where no genuine coordination benefit exists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_openclaw_regulation,
    'Will the benefits of regulating OpenClaw outweigh the potential stifling of innovation?',
    'Longitudinal studies tracking the impact of regulation on AI development and adoption rates.',
    'If True: Sustainable AI ecosystem with user protection. If False: Stifled innovation and loss of competitive advantage.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(openclaw_regulation, 0, 10).

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
narrative_ontology:measurement(openclaw_regulation_tr_t0, openclaw_regulation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(openclaw_regulation_tr_t5, openclaw_regulation, theater_ratio, 5, 0.20).
narrative_ontology:measurement(openclaw_regulation_tr_t10, openclaw_regulation, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(openclaw_regulation_ex_t0, openclaw_regulation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(openclaw_regulation_ex_t5, openclaw_regulation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(openclaw_regulation_ex_t10, openclaw_regulation, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */