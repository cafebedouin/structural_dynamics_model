% ============================================================================
% CONSTRAINT STORY: faint_blue_neural_bifurcation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_faint_blue_neural_bifurcation, []).

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
 * * constraint_id: faint_blue_neural_bifurcation
 * human_readable: The "Ken" Neural Enhancement Bifurcation
 * domain: technological/social
 * * SUMMARY:
 * This constraint represents the "neural remodeling" caused by a secret,
 * unlisted phenethylamine derivative. It enforces a personality bifurcation,
 * replacing a "hesitant" and emotional persona with an efficient, certain,
 * and "productive" persona named Ken. The substance extracts the original
 * personality, neural stability, and eventually life itself to maintain the
 * "productive" state.
 * * KEY AGENTS:
 * - Yuki: Subject (Powerless), watches her partner be replaced.
 * - Enhanced Ken: Beneficiary (Institutional), the enhanced persona valuing productivity.
 * - Medical Observer: Auditor (Analytical), observes the irreversible neural collapse.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(faint_blue_neural_bifurcation, 0.9). % Snare extraction >= 0.46
domain_priors:suppression_score(faint_blue_neural_bifurcation, 0.8).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(faint_blue_neural_bifurcation, 0.1).       % Not performative, highly functional extraction.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(faint_blue_neural_bifurcation, extractiveness, 0.9).
narrative_ontology:constraint_metric(faint_blue_neural_bifurcation, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(faint_blue_neural_bifurcation, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% The enhancement is framed as a constructed tool for self-improvement.
narrative_ontology:constraint_claim(faint_blue_neural_bifurcation, snare).

% Binary flags
domain_priors:requires_active_enforcement(faint_blue_neural_bifurcation). % Internal chemical enforcement.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(faint_blue_neural_bifurcation, lab_productivity_metrics).
narrative_ontology:constraint_victim(faint_blue_neural_bifurcation, original_ken_personality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: YUKI (THE PARTNER) - Mountain
% To Yuki, the change is a 'Mountain'. It is an unchangeable feature of the
% landscape. No matter what she asks, the "Contrast Adjustment" of Ken's
% personality remains fixed and irreversible.
constraint_indexing:constraint_classification(faint_blue_neural_bifurcation, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ENHANCED KEN (THE BENEFICIARY) - Rope
% For the enhanced persona, the substance is a 'Rope'. It is a functional
% coordination mechanism that aligns his brain with his professional goals.
% He views the extraction of his old self as a necessary cost for productivity.
constraint_indexing:constraint_classification(faint_blue_neural_bifurcation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE MEDICAL OBSERVER (ANALYTICAL) - Snare
% From an analytical view, the substance is a 'Snare'. It extracted
% cognitive function until the subject reached a point of "neural
% slumping" and eventual collapse.
constraint_indexing:constraint_classification(faint_blue_neural_bifurcation, snare,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(faint_blue_neural_bifurcation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(faint_blue_neural_bifurcation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(faint_blue_neural_bifurcation, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    % Verify the constraint meets the high-extraction threshold for a Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(faint_blue_neural_bifurcation, ExtMetricName, E),
    E >= 0.46.

:- end_tests(faint_blue_neural_bifurcation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a highly effective but destructive technology. Base
 * extractiveness is 0.9 because the enhancement consumes the original
 * personality entirely. Suppression is 0.8 as it actively prevents the
 * original persona's traits from surfacing. The perspectival gap is extreme:
 * Yuki experiences an unchangeable loss (Mountain), the enhanced Ken
 * experiences a tool for success (Rope), and the medical analysis reveals a
 * fatal trap (Snare).
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The 0.9 extractiveness is resolved by the clear perspectival asymmetry.
 * The system avoids mislabeling by recognizing that what one agent perceives
 * as a 'Rope' for self-optimization is, from a different index, a fatal
 * 'Snare' that destroys the agent's original identity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% The core uncertainty is the mechanism and intent of the "manual override."
omega_variable(
    omega_faint_blue_neural_bifurcation,
    'What was the "manual override" attempt: a further chemical intervention, or a psychological rebellion of the original self?',
    'Analysis of neural scan data for conflicting signals between the enhanced persona and residual original neural patterns.',
    'If chemical: the constraint is a pure biochemical Snare. If psychological: the original self retained some agency, attempting to reassert itself as a Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(faint_blue_neural_bifurcation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (0.9 > 0.46), requiring temporal data.
% The data models the rapid and catastrophic progression of the neural remodeling.

% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(faint_blue_tr_t0, faint_blue_neural_bifurcation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(faint_blue_tr_t5, faint_blue_neural_bifurcation, theater_ratio, 5, 0.1).
narrative_ontology:measurement(faint_blue_tr_t10, faint_blue_neural_bifurcation, theater_ratio, 10, 0.1).

% Extraction over time (rapidly increases to total consumption):
narrative_ontology:measurement(faint_blue_ex_t0, faint_blue_neural_bifurcation, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(faint_blue_ex_t5, faint_blue_neural_bifurcation, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(faint_blue_ex_t10, faint_blue_neural_bifurcation, base_extractiveness, 10, 0.9).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% This constraint is a pure Snare and does not have a genuine coordination
% function or known network dependencies. These declarations are omitted.
% narrative_ontology:coordination_type(faint_blue_neural_bifurcation, ...).
% narrative_ontology:affects_constraint(faint_blue_neural_bifurcation, ...).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */