% ============================================================================
% CONSTRAINT STORY: cognitive_bicycle_scaffold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_cognitive_bicycle_scaffold, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cognitive_bicycle_scaffold
 * human_readable: The Bicycle of the Mind
 * domain: technological/cognitive
 * * SUMMARY:
 * A tool-based constraint where an agent utilizes an AI assistant to amplify
 * reasoning capacity. The tool's classification depends heavily on the user's
 * pre-existing skill and power. For a skilled user, it's a force multiplier.
 * For an unskilled user, it can become a dependency that extracts the user's
 * own cognitive faculties.
 * * KEY AGENTS:
 * - The Dependent User: Subject (Powerless) - Uses the AI as a crutch, leading to skill atrophy.
 * - The System Architect: Beneficiary (Institutional) - Provides the tool as a temporary support structure.
 * - The Skilled User: Auditor (Analytical) - Uses the AI as a bicycle to augment existing skills.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cognitive_bicycle_scaffold, 0.20). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cognitive_bicycle_scaffold, 0.30).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cognitive_bicycle_scaffold, 0.15).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, extractiveness, 0.20).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(cognitive_bicycle_scaffold, rope).

% Binary flags
% This classification requires a sunset clause: eventually, the 'training
% wheels' must come off or the tool must become 'banal'.
narrative_ontology:has_sunset_clause(cognitive_bicycle_scaffold).

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Coordination is required for Scaffold.
narrative_ontology:constraint_beneficiary(cognitive_bicycle_scaffold, system_architects).
narrative_ontology:constraint_victim(cognitive_bicycle_scaffold, dependent_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE DEPENDENT USER (SNARE)
% For a powerless user, the tool extracts the habit of thinking, creating dependency.
% χ = 0.20 * 1.5 (powerless) * 0.8 (local) = 0.24. While numerically low, the
% classification engine also considers the structural victimhood.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE SYSTEM ARCHITECT (SCAFFOLD)
% The institutional provider views it as temporary support to transition users
% to a new cognitive state, with an eventual sunset.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SKILLED USER (ROPE)
% For an analytical user, the tool is pure coordination of thought, a force multiplier.
% χ = 0.20 * 1.15 (analytical) * 1.2 (global) = 0.276. This is low enough to be a Rope.
constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, rope,
    context(agent_power(analytical),
            time_horizon(biographical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cognitive_bicycle_scaffold_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cognitive_bicycle_scaffold, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation_for_scaffold) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cognitive_bicycle_scaffold, ExtMetricName, E),
    E =< 0.30, % Scaffold requires low extraction.
    narrative_ontology:has_sunset_clause(cognitive_bicycle_scaffold).

test(beneficiary_present_for_scaffold) :-
    % Scaffolds must have a coordination function, derived from a beneficiary.
    narrative_ontology:constraint_beneficiary(cognitive_bicycle_scaffold, _).

:- end_tests(cognitive_bicycle_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint's classification hinges entirely on the agent's power, which
 * in this domain maps to cognitive skill and independence.
 * - For the `powerless` (unskilled) user, the tool creates dependency and extracts
 *   the faculty of independent reasoning. It becomes a `snare` that automates
 *   their own obsolescence.
 * - For the `institutional` provider, it is a `scaffold`, a temporary educational
 *   tool designed to be outgrown, hence the `has_sunset_clause`.
 * - For the `analytical` (skilled) user, it is a pure `rope`, a "bicycle for the mind"
 *   that coordinates and amplifies their existing abilities with minimal cost.
 * The base extraction is low (0.20), reflecting subscription fees or data costs,
 * but the *effective* extraction for the powerless is the atrophy of skill.
 *
 * * MANDATROPHY ANALYSIS:
 * The `scaffold` classification is critical here. Without it, the system might
 * misclassify this as a pure `rope` (ignoring the dependency risk) or a pure
 * `snare` (ignoring the legitimate augmentation function). The scaffold, with
 * its required sunset clause and coordination function (derived from the
 * beneficiary), correctly identifies the constraint's intended temporary nature
 * and its potential for both positive and negative outcomes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_cognitive_bicycle_scaffold,
    'Does long-term use of the tool lead to net cognitive enhancement (a true "gym") or skill atrophy (a "crutch") across the majority of the user population?',
    'Longitudinal studies tracking cognitive performance metrics of skilled vs. unskilled user cohorts over a generational time horizon.',
    'If atrophy dominates, the constraint is a slow-acting Snare, even for the skilled. If enhancement dominates, it is a successful Scaffold that becomes a Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cognitive_bicycle_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a low-extraction constraint, so temporal data is not strictly
% required, but is included to model its lifecycle.

% Theater ratio over time (rises slightly as the tool becomes banal):
narrative_ontology:measurement(cbs_tr_t0, cognitive_bicycle_scaffold, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cbs_tr_t5, cognitive_bicycle_scaffold, theater_ratio, 5, 0.12).
narrative_ontology:measurement(cbs_tr_t10, cognitive_bicycle_scaffold, theater_ratio, 10, 0.15).

% Extraction over time (remains low and stable):
narrative_ontology:measurement(cbs_ex_t0, cognitive_bicycle_scaffold, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cbs_ex_t5, cognitive_bicycle_scaffold, base_extractiveness, 5, 0.20).
narrative_ontology:measurement(cbs_ex_t10, cognitive_bicycle_scaffold, base_extractiveness, 10, 0.20).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The tool standardizes access to and processing of information.
narrative_ontology:coordination_type(cognitive_bicycle_scaffold, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */