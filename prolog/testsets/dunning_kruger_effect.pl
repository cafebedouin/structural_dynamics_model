% ============================================================================
% CONSTRAINT STORY: dunning_kruger_effect
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_dunning_kruger_effect, []).

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
 * * constraint_id: dunning_kruger_effect
 * human_readable: Dunning-Kruger Effect (Cognitive Bias of Self-Assessment)
 * domain: social/cognitive
 * * SUMMARY:
 * The Dunning-Kruger effect is a cognitive bias where people with limited
 * competence in a domain overestimate their own ability. This occurs because
 * the skills required for competence are the same skills needed to recognize
 * incompetence. The bias extracts resources (via errors, wasted time) from
 * organizations and systems that rely on accurate self-assessment for efficiency.
 * * KEY AGENTS:
 * - The Novice: Subject (Powerless), trapped by their own lack of meta-cognition.
 * - The Organization: Beneficiary (Institutional), can leverage understanding of the bias for training.
 * - The Analytical Observer: Auditor (Analytical), models the system-wide effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds. Scores adjusted to reflect a Tangled Rope.
domain_priors:base_extractiveness(dunning_kruger_effect, 0.55). % ε ≥ 0.50 for Tangled Rope. Represents wasted resources and opportunity costs from errors.
domain_priors:suppression_score(dunning_kruger_effect, 0.75).   % The bias itself suppresses the visibility of competence, making alternatives hard to perceive.
domain_priors:theater_ratio(dunning_kruger_effect, 0.05).       % The effect is a genuine cognitive function, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(dunning_kruger_effect, extractiveness, 0.55).
narrative_ontology:constraint_metric(dunning_kruger_effect, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(dunning_kruger_effect, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% It is a constructed feature of human cognition, not a natural law of physics.
narrative_ontology:constraint_claim(dunning_kruger_effect, tangled_rope).

% Binary flags
% The "enforcement" is the cognitive architecture itself, reinforced by social
% systems that fail to provide corrective feedback. Required for Tangled Rope.
domain_priors:requires_active_enforcement(dunning_kruger_effect).

% Structural property derivation hooks:
% Both beneficiary and victim are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(dunning_kruger_effect, overconfident_novices). % Beneficiary in terms of unearned confidence.
narrative_ontology:constraint_victim(dunning_kruger_effect, organizational_efficiency). % Victim is the system suffering from errors.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The novice is trapped by their own lack of meta-cognition. The high
% effective extraction (χ = 0.55 * 1.5 * 0.8 = 0.66) represents the severe
% opportunity cost and potential for catastrophic error they are unaware of.
constraint_indexing:constraint_classification(dunning_kruger_effect, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% For an organization that understands the bias, it becomes a pure coordination
% tool. The negative effective extraction (χ = 0.55 * -0.2 * 1.0 = -0.11) shows
% that knowledge of the constraint *adds* value, allowing for better training
% programs, mentorship, and team composition. It's a Rope for managing human capital.
constraint_indexing:constraint_classification(dunning_kruger_effect, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (managing talent) and the
% asymmetric extraction (wasted resources). The high effective extraction
% (χ = 0.55 * 1.15 * 1.2 = 0.759) combined with the structural properties
% (beneficiary, victim, enforcement) correctly identifies it as a Tangled Rope.
constraint_indexing:constraint_classification(dunning_kruger_effect, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dunning_kruger_effect_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(dunning_kruger_effect, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dunning_kruger_effect, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dunning_kruger_effect, ExtMetricName, E),
    (E =< 0.15 ; E >= 0.46),
    assertion(E >= 0.46). % Ensures it's a high-extraction Snare/Tangled Rope.

test(tangled_rope_structural_properties) :-
    % Verify all three required properties for Tangled Rope are present.
    domain_priors:requires_active_enforcement(dunning_kruger_effect),
    narrative_ontology:constraint_beneficiary(dunning_kruger_effect, _),
    narrative_ontology:constraint_victim(dunning_kruger_effect, _).

:- end_tests(dunning_kruger_effect_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Dunning-Kruger effect is modeled as a Tangled Rope from an analytical
 * perspective because it has both a coordination function and an extractive one.
 * The base extractiveness (0.55) is high, representing the significant waste
 * and error caused by the bias in any system reliant on skill. The suppression
 * (0.75) is also high, as the bias's core mechanism is to prevent an individual
 * from perceiving their own incompetence.
 *
 * The Perspectival Gap is stark:
 * - For the 'powerless' novice, it's a Snare. They are trapped by a cognitive
 *   blind spot that extracts potential and exposes them to risk.
 * - For the 'institutional' organization, knowledge of the effect is a Rope.
 *   It's a tool for coordinating training, mentorship, and performance reviews,
 *   turning a cognitive flaw into a manageable variable.
 * - The 'analytical' view synthesizes these, seeing the dual nature of coordination
 *   and extraction, which is the definition of a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] Classifying this as a pure Snare would be inaccurate,
 * as it would miss the coordination utility that institutions derive from
 * understanding and managing the bias. The Tangled Rope classification correctly
 * captures that this cognitive feature, while harmful if ignored, becomes a
 * tool for social coordination when explicitly addressed, preventing a
 * misclassification of a complex social dynamic as pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_dunning_kruger_effect,
    'Is the Dunning-Kruger effect a fundamental cognitive bias or a mathematical artifact of regression toward the mean?',
    'Replication of studies using non-traditional statistical models, coupled with neurological imaging of self-assessment processes.',
    'If an artifact, the constraint is a Piton of flawed science. If a true bias, it is a permanent Tangled Rope in human systems.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(dunning_kruger_effect, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is required as base_extractiveness > 0.46.
% The Dunning-Kruger effect is modeled as a stable feature of human cognition
% over the interval, so the values do not drift.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(dke_tr_t0, dunning_kruger_effect, theater_ratio, 0, 0.05).
narrative_ontology:measurement(dke_tr_t5, dunning_kruger_effect, theater_ratio, 5, 0.05).
narrative_ontology:measurement(dke_tr_t10, dunning_kruger_effect, theater_ratio, 10, 0.05).

% Extraction over time (stable and high):
narrative_ontology:measurement(dke_ex_t0, dunning_kruger_effect, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(dke_ex_t5, dunning_kruger_effect, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(dke_ex_t10, dunning_kruger_effect, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% No Boltzmann or Network data is declared for this constraint as it is a
% cognitive bias rather than a formal institutional mechanism.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */