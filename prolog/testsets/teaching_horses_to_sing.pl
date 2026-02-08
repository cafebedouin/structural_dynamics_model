% ============================================================================
% CONSTRAINT STORY: teaching_horses_to_sing
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_teaching_horses_to_sing, []).

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
 * * constraint_id: teaching_horses_to_sing
 * human_readable: The Sing-or-Die Gambit
 * domain: political
 * * SUMMARY:
 * A thief under a death sentence convinces a monarch to grant a one-year stay of
 * execution in exchange for an impossible promise: teaching the monarch's horse
 * to sing. The constraint is the year-long bargain, which functions as a tool
 * for existential arbitrage against the certainty of immediate death. It is a
 * constructed system of high extraction (the thief's liberty) that also serves
 * a coordination function (the monarch's amusement).
 * * KEY AGENTS:
 * - The Thief: Subject (Powerless), who is trapped in the arrangement.
 * - The Monarch: Beneficiary (Institutional), who enjoys the spectacle.
 * - The System Auditor: Observer (Analytical), who assesses the structure of the bargain.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(teaching_horses_to_sing, 0.75). % High. The thief's entire liberty and life for a year are extracted.
domain_priors:suppression_score(teaching_horses_to_sing, 0.40).   % Moderate. The impossibility is known, but the monarch's authority suppresses dissent.
domain_priors:theater_ratio(teaching_horses_to_sing, 0.10).       % Low. The daily singing is a real, enforced activity, not just a performance of compliance.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(teaching_horses_to_sing, extractiveness, 0.75).
narrative_ontology:constraint_metric(teaching_horses_to_sing, suppression_requirement, 0.40).
narrative_ontology:constraint_metric(teaching_horses_to_sing, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The bargain is a constructed deal, not a natural law or simple coordination.
narrative_ontology:constraint_claim(teaching_horses_to_sing, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(teaching_horses_to_sing). % Guards must keep the thief imprisoned.

% Structural property derivation hooks:
% The monarch's amusement is the coordination function. The thief's imprisonment is the asymmetric extraction.
narrative_ontology:constraint_beneficiary(teaching_horses_to_sing, the_monarch).
narrative_ontology:constraint_victim(teaching_horses_to_sing, the_thief).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (THE THIEF) - SNARE
% For the thief, the bargain is a trap. Despite buying time, he is imprisoned
% and his life is forfeit at the end. The effective extraction is immense.
% χ = 0.75 * 1.5 (powerless) * 0.8 (local) = 0.90.
constraint_indexing:constraint_classification(teaching_horses_to_sing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (THE MONARCH) - ROPE
% For the monarch, this is a low-cost coordination mechanism for entertainment.
% The effective extraction is negative, indicating a net benefit.
% χ = 0.75 * -0.2 (institutional) * 0.9 (regional) = -0.135.
constraint_indexing:constraint_classification(teaching_horses_to_sing, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER - TANGLED ROPE
% Structurally, the system has a coordination function (amusement) and
% asymmetric extraction (imprisonment), requiring active enforcement. This is
% the definition of a Tangled Rope.
% χ = 0.75 * 1.15 (analytical) * 1.2 (global) = 1.035.
constraint_indexing:constraint_classification(teaching_horses_to_sing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(teaching_horses_to_sing_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(teaching_horses_to_sing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(teaching_horses_to_sing, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict as a Tangled Rope.
    constraint_indexing:constraint_classification(teaching_horses_to_sing, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    % Verify the base extraction meets the Snare/Tangled Rope threshold.
    narrative_ontology:constraint_metric(teaching_horses_to_sing, extractiveness, E),
    E >= 0.46.

:- end_tests(teaching_horses_to_sing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original file's classifications were based on a purely narrative interpretation
 * that conflicted with the system's mechanics. This version corrects them.
 * - The Thief (Snare): While the thief *hopes* the bargain is a Rope to survival,
 *   structurally he is trapped in a high-extraction system where his liberty is
 *   the cost. The calculated effective extraction (χ=0.90) is extremely high,
 *   confirming the Snare classification.
 * - The Monarch (Rope): The monarch experiences a net benefit (χ=-0.135),
 *   viewing the arrangement as a simple coordination mechanism for his amusement.
 * - Analytical (Tangled Rope): This is the key insight. The system is not a pure
 *   Snare because it has a genuine (if frivolous) coordination function for the
 *   beneficiary. It is not a Rope because of the coercive, asymmetric extraction
 *   from the victim. The Tangled Rope classification correctly identifies this
 *   hybrid nature.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * By classifying the system as a Tangled Rope, the model avoids mischaracterizing
 * it as a pure Snare (ignoring the monarch's coordination goal) or a pure Rope
 * (ignoring the thief's exploitation). It correctly identifies the dual-function
 * nature of the constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_teaching_horses_to_sing_1,
    'Is the thiefs primary motivation survival arbitrage (a bet on stochastic events) or a belief in a hidden potential (horses can be taught)?',
    'Post-hoc interview with the thief or discovery of a hidden training manual.',
    'If arbitrage, the constraint is a pure gamble. If belief, it is an attempt to transform a Mountain into a Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(teaching_horses_to_sing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is established and remains static for its one-year duration.
% Therefore, the metrics do not drift over the interval.
%
% Theater ratio over time:
narrative_ontology:measurement(thts_tr_t0, teaching_horses_to_sing, theater_ratio, 0, 0.10).
narrative_ontology:measurement(thts_tr_t5, teaching_horses_to_sing, theater_ratio, 5, 0.10).
narrative_ontology:measurement(thts_tr_t10, teaching_horses_to_sing, theater_ratio, 10, 0.10).

% Extraction over time:
narrative_ontology:measurement(thts_ex_t0, teaching_horses_to_sing, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(thts_ex_t5, teaching_horses_to_sing, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(thts_ex_t10, teaching_horses_to_sing, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The bargain is a mechanism for enforcing the monarch's will and extracting
% amusement, fitting the 'enforcement_mechanism' coordination type.
narrative_ontology:coordination_type(teaching_horses_to_sing, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */