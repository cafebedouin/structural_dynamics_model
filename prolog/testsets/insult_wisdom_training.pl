% ============================================================================
% CONSTRAINT STORY: insult_wisdom_training
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_insult_wisdom_training, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: insult_wisdom_training
 * human_readable: The Odd Assignment (Paying for Insults)
 * domain: religious/social/philosophical
 * * SUMMARY:
 * A disciple is commanded by his Master to pay anyone who insults him for three years as a prerequisite for wisdom. This artificial constraint forces a radical decoupling of ego from social feedback, transforming a galling extraction into a state of "free" resilience where insults no longer have coercive power. The high extraction is justified by its pedagogical function, which is only apparent over a longer time horizon.
 * * KEY AGENTS:
 * - The Disciple: Subject (Powerless) who undergoes the trial.
 * - The Master: Beneficiary (Institutional) who designs the trial as a tool.
 * - Analytical Observer: Auditor who assesses the structure of the trial.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(insult_wisdom_training, 0.75). % High extraction: financial and emotional cost.
domain_priors:suppression_score(insult_wisdom_training, 0.50).   % Suppresses natural self-defense instinct.
domain_priors:theater_ratio(insult_wisdom_training, 0.05).       % Highly functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(insult_wisdom_training, extractiveness, 0.75).
narrative_ontology:constraint_metric(insult_wisdom_training, suppression_requirement, 0.50).
narrative_ontology:constraint_metric(insult_wisdom_training, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% It is a constructed pedagogical tool, not a natural law.
narrative_ontology:constraint_claim(insult_wisdom_training, tangled_rope).
narrative_ontology:human_readable(insult_wisdom_training, "The Odd Assignment (Paying for Insults)").

% Binary flags
domain_priors:requires_active_enforcement(insult_wisdom_training). % The Master's command must be obeyed.

% Structural property derivation hooks:
% The disciple's future self (wisdom) is the beneficiary.
narrative_ontology:constraint_beneficiary(insult_wisdom_training, disciple_future_self).
% The disciple's present self (ego, finances) is the victim.
narrative_ontology:constraint_victim(insult_wisdom_training, disciple_present_self).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE DISCIPLE (DURING THE TRIAL)
% From an immediate, trapped perspective, the constant extraction of money and
% dignity feels like a predatory trap with no escape.
constraint_indexing:constraint_classification(insult_wisdom_training, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MASTER (THE ARCHITECT/BENEFICIARY)
% From the institutional perspective of the teacher, the constraint is a
% functional tool (a Rope) for achieving a specific outcome. The high
% extraction is seen as a necessary cost for the benefit it produces.
% The institutional power modifier π(-0.2) makes effective extraction negative.
constraint_indexing:constraint_classification(insult_wisdom_training, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (POST-HOC)
% An analyst sees both the genuine coordination function (recalibrating the
% ego) and the asymmetric extraction. It requires active enforcement and has
% clear beneficiaries and victims. This is the definition of a Tangled Rope.
constraint_indexing:constraint_classification(insult_wisdom_training, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(insult_wisdom_training_tests).

test(perspectival_gap_snare_to_rope) :-
    % Verify the key perspectival gap between the subject and the beneficiary.
    constraint_indexing:constraint_classification(insult_wisdom_training, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(insult_wisdom_training, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(insult_wisdom_training, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    % Verify the base extractiveness is in the high-extraction range for a Snare/Tangled Rope.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(insult_wisdom_training, ExtMetricName, E),
    E >= 0.46.

:- end_tests(insult_wisdom_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this story is the transformation of a constraint's perceived nature
 * through time and perspective. The base extractiveness is set high (0.75) to
 * reflect the literal, sustained cost (financial and emotional) to the disciple.
 *
 * The Perspectival Gap is stark:
 * - For the 'powerless' disciple in the 'immediate' timeframe, it is an inescapable Snare.
 * - For the 'institutional' Master, it is a functional Rope—a tool for producing wisdom.
 * - For the 'analytical' observer, its dual nature (coordination + extraction) makes it a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED]
 * The high extraction (0.75) poses a risk of being a Mandatrophy (a purely
 * extractive system with no function). This is resolved by the time horizon.
 * The constraint's function is not immediate gratification but long-term
 * transformation. The final scene, where the disciple laughs at a "free"
 * insult, proves the pedagogical function was real. The Snare was, in fact,
 * a component of a larger, functional Tangled Rope designed for a specific
 * purpose. The system correctly identifies this by classifying it differently
 * across time horizons and power levels.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_insult_wisdom_training,
    'Is the three-year trial a functional necessity for ego-liquidation, or an arbitrary hazing ritual by the Master?',
    'Comparative analysis of pedagogical outcomes from this method versus less extractive methods.',
    'If necessity, it is a harsh but valid Tangled Rope. If arbitrary, it is a pure Snare disguised as pedagogy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(insult_wisdom_training, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (E=0.75), requiring temporal data.
% The constraint itself is static during the three-year trial; the disciple's
% perception of it changes. Therefore, the base metrics are constant.
%
% Theater ratio over time (consistently low and functional):
narrative_ontology:measurement(iwt_tr_t0, insult_wisdom_training, theater_ratio, 0, 0.05).
narrative_ontology:measurement(iwt_tr_t5, insult_wisdom_training, theater_ratio, 5, 0.05).
narrative_ontology:measurement(iwt_tr_t10, insult_wisdom_training, theater_ratio, 10, 0.05).

% Extraction over time (consistently high and costly):
narrative_ontology:measurement(iwt_ex_t0, insult_wisdom_training, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(iwt_ex_t5, insult_wisdom_training, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(iwt_ex_t10, insult_wisdom_training, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint functions as a tool to re-allocate the disciple's internal
% resources (attention, emotional energy) away from ego-defense.
narrative_ontology:coordination_type(insult_wisdom_training, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */