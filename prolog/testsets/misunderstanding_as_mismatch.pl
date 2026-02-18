% ============================================================================
% CONSTRAINT STORY: misunderstanding_as_mismatch
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_misunderstanding_as_mismatch, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: misunderstanding_as_mismatch
 * human_readable: Social Pressure for Worldview Assimilation
 * domain: social/psychological
 * * SUMMARY:
 * This constraint models the social pressure for individuals to assimilate into a group's dominant worldview.
 * Dissent or non-consensus views are treated as disruptive, leading to social exclusion. This dynamic extracts
 * individual autonomy in exchange for group cohesion, creating a trade-off between social belonging (short-term status)
 * and holding a potentially more accurate, non-consensus belief (long-term status).
 * * KEY AGENTS:
 * - Individual Dissenter: Subject (Powerless), faces social exclusion for holding non-consensus views.
 * - Ideological Group: Beneficiary (Institutional), enforces worldview for cohesion and control.
 * - Analytical Observer: Auditor (Analytical), assesses the system's structural properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(misunderstanding_as_mismatch, 0.75). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(misunderstanding_as_mismatch, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(misunderstanding_as_mismatch, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, extractiveness, 0.75).
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(misunderstanding_as_mismatch, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The group claims this pressure is for beneficial coordination and cohesion.
narrative_ontology:constraint_claim(misunderstanding_as_mismatch, tangled_rope).
narrative_ontology:human_readable(misunderstanding_as_mismatch, "Social Pressure for Worldview Assimilation").
narrative_ontology:topic_domain(misunderstanding_as_mismatch, "social/psychological").

% Binary flags
domain_priors:requires_active_enforcement(misunderstanding_as_mismatch). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(misunderstanding_as_mismatch, ideological_group).
narrative_ontology:constraint_victim(misunderstanding_as_mismatch, individual_dissenters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual dissenter feels trapped by the need for social belonging.
% Dissent leads to exclusion, extracting their social capital and autonomy.
% χ = 0.75 * 1.5 (powerless) * 0.8 (local) = 0.90. This is a clear Snare.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The ideological group sees this as a necessary Rope for coordination and cohesion.
% The negative extraction from their institutional power makes it appear purely functional.
% χ = 0.75 * -0.2 (institutional) * 1.0 (national) = -0.15. This is a clear Rope.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function and the asymmetric extraction.
% It serves a group purpose but at a high, coercive cost to a specific victim class.
% χ = 0.75 * 1.15 (analytical) * 1.2 (global) = 1.035. High extraction.
% With high extraction, high suppression, enforcement, and both beneficiary/victim, it's a Tangled Rope.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE STRATEGIC VISIONARY (ROPE)
% A powerful individual can leverage being misunderstood as a strategic tool (Rope)
% to pursue non-consensus bets, trading short-term status for long-term gains.
% χ = 0.75 * 0.6 (powerful) * 1.2 (global) = 0.54. Felt extraction is lower.
constraint_indexing:constraint_classification(misunderstanding_as_mismatch, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(misunderstanding_as_mismatch_tests).

test(perspectival_gap_subject_beneficiary) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_view_is_tangled_rope) :-
    % The analytical observer must correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(misunderstanding_as_mismatch, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    % Verify the base extraction is high enough to be a Snare/Tangled Rope.
    domain_priors:base_extractiveness(misunderstanding_as_mismatch, E),
    E >= 0.46.

:- end_tests(misunderstanding_as_mismatch_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base scores reflect a highly coercive social dynamic. Extraction (0.75) represents the loss of
 * individual autonomy and social capital for dissenters. Suppression (0.70) reflects the active shunning
 * and exclusion mechanisms used by the group.
 *
 * The Perspectival Gap is stark:
 * - For the 'powerless' dissenter, this is a Snare. The cost of non-conformity is immense, and exit is socially catastrophic.
 * - For the 'institutional' group, it's a Rope. The mechanism is essential for maintaining cohesion and power, and the negative extraction
 *   modifier for institutional power means they perceive no cost, only benefit.
 * - The 'analytical' observer, seeing both the coordination function for the group and the severe, asymmetric extraction
 *   from dissenters, correctly classifies it as a Tangled Rope. This classification is critical as it acknowledges the
 *   dual nature of the constraint, preventing a misclassification as either pure coordination (Rope) or pure predation (Snare).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction is resolved by the Tangled Rope classification. The system does not simply label this a Snare,
 * which would ignore the genuine (from the beneficiary's perspective) coordination function. By identifying it as a
 * Tangled Rope, the system acknowledges the group cohesion benefit while correctly flagging the coercive, extractive
 * cost imposed on victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_misunderstanding_as_mismatch,
    'Does "truth" actually prevail over biographical timescales, allowing a visionary''s non-consensus bet to succeed, or is the assumption of long-term high-status a narrative fallacy?',
    'Longitudinal tracking of non-consensus bets vs. actual historical consensus 50 years later; analysis of reputational shifts in controversial figures.',
    'If truth converges, the dynamic can be a strategic Rope for the powerful. If consensus is path-dependent and arbitrary, it remains a terminal Snare for almost everyone.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(misunderstanding_as_mismatch, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This constraint is modeled as being
% consistently severe, a perennial feature of social dynamics, with slight
% intensification over time as information cascades accelerate.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (stable and low):
narrative_ontology:measurement(misunderstanding_tr_t0, misunderstanding_as_mismatch, theater_ratio, 0, 0.10).
narrative_ontology:measurement(misunderstanding_tr_t5, misunderstanding_as_mismatch, theater_ratio, 5, 0.10).
narrative_ontology:measurement(misunderstanding_tr_t10, misunderstanding_as_mismatch, theater_ratio, 10, 0.10).

% Extraction over time (starts high and slightly increases):
narrative_ontology:measurement(misunderstanding_ex_t0, misunderstanding_as_mismatch, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(misunderstanding_ex_t5, misunderstanding_as_mismatch, base_extractiveness, 5, 0.74).
narrative_ontology:measurement(misunderstanding_ex_t10, misunderstanding_as_mismatch, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint functions by enforcing a group norm or worldview.
narrative_ontology:coordination_type(misunderstanding_as_mismatch, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */