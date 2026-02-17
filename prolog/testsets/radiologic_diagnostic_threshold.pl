% ============================================================================
% CONSTRAINT STORY: radiologic_diagnostic_threshold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_radiologic_diagnostic_threshold, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: radiologic_diagnostic_threshold
 * human_readable: The Radiologic Diagnostic Threshold
 * domain: medical/technological/legal
 * * SUMMARY:
 * This constraint governs the classification of medical imaging findings into "benign," "incidental," or "critical."
 * It is not a purely biological fact but an indexical classification dependent on the reader's
 * power level, legal exposure, and technological tools. Findings oscillate between "ignore" and "urgent"
 * as contextual priors change. Frontline workers experience the threshold as a Snare of liability,
 * while institutions present it as a Mountain of objective safety.
 * * KEY AGENTS:
 * - Frontline Radiologist: Subject (Powerless), facing high volume and legal pressure.
 * - Hospital Administrator/Guideline Author: Beneficiary (Institutional), defining "standard of care".
 * - Retrospective Audit Panel: Auditor (Analytical), reclassifying past ambiguity with hindsight.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(radiologic_diagnostic_threshold, 0.70). % High extraction from cognitive load and liability shift.
domain_priors:suppression_score(radiologic_diagnostic_threshold, 0.60).   % Alternatives (e.g., probabilistic reporting) are suppressed by legal/billing systems.
domain_priors:theater_ratio(radiologic_diagnostic_threshold, 0.15).       % Low theater; the system is brutally functional, not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, extractiveness, 0.70).
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be an objective, scientific fact (a Mountain).
narrative_ontology:constraint_claim(radiologic_diagnostic_threshold, tangled_rope).
narrative_ontology:human_readable(radiologic_diagnostic_threshold, "The Radiologic Diagnostic Threshold").

% Binary flags
domain_priors:requires_active_enforcement(radiologic_diagnostic_threshold). % Malpractice litigation and peer review enforce the thresholds.

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(radiologic_diagnostic_threshold, malpractice_insurers).
narrative_ontology:constraint_beneficiary(radiologic_diagnostic_threshold, hospital_administrators).
narrative_ontology:constraint_victim(radiologic_diagnostic_threshold, frontline_radiologists).
narrative_ontology:constraint_victim(radiologic_diagnostic_threshold, overdiagnosed_patients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The frontline radiologist, facing high volume and legal risk, experiences
% the threshold as a coercive liability trap.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The hospital administrator or guideline author views the threshold as a
% necessary coordination tool for standardizing care and managing risk.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system auditor sees both the coordination function and the asymmetric
% extraction, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(radiologic_diagnostic_threshold_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(radiologic_diagnostic_threshold, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_high_extraction) :-
    % Verify the base extractiveness is in the Snare/Tangled Rope range.
    narrative_ontology:constraint_metric(radiologic_diagnostic_threshold, extractiveness, E),
    E >= 0.46.

:- end_tests(radiologic_diagnostic_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that began as a coordination mechanism (Rope) but has
 * become highly extractive. The base extractiveness of 0.70 captures the intense
 * cognitive and legal burden shifted onto frontline radiologists (the "liability tax").
 * The suppression score of 0.60 reflects how legal and billing systems actively punish
 * more nuanced, probabilistic approaches, locking in the current paradigm.
 *
 * The Perspectival Gap is stark:
 * - The Radiologist (powerless) experiences a Snare. Any deviation from the rigid
 *   threshold creates immense personal risk.
 * - The Administrator (institutional) sees a Rope. The thresholds are essential tools
 *   for standardizing care, managing throughput, and defending against litigation.
 * - The Analytical view reveals the Tangled Rope. It is a system with a genuine
 *   coordination function that has been co-opted for asymmetric risk transfer.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * A simple analysis might label this system a pure Snare, focusing only on the
 * harm to clinicians. However, this would be a mandatrophy, ignoring the genuine
 * coordination problem the thresholds solve (i.e., preventing chaotic, non-standard
 * diagnostic practices). The Tangled Rope classification correctly captures this
 * duality: it acknowledges the vital coordination function (the Rope aspect) while
 * simultaneously flagging the severe, asymmetric extraction (the Snare aspect) that
 * makes the system pathological. This prevents the system from recommending a naive
 * "dismantle" solution that would ignore the underlying coordination need.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_radiologic_diagnostic_threshold,
    'Is there an objective, context-free "correct" diagnostic threshold, or is it purely a social/legal construct?',
    'Longitudinal outcome studies comparing systems with different thresholds vs. analysis of legal precedent evolution.',
    'If objective (Mountain), errors are absolute failures. If constructed (Tangled Rope), errors are coordination failures.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(radiologic_diagnostic_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified as malpractice pressure and patient volume grew.
% The data models extraction accumulation over the last two decades.
%
% Theater ratio over time (slight increase due to bureaucratic checklists):
narrative_ontology:measurement(rdt_tr_t0, radiologic_diagnostic_threshold, theater_ratio, 0, 0.05).
narrative_ontology:measurement(rdt_tr_t5, radiologic_diagnostic_threshold, theater_ratio, 5, 0.10).
narrative_ontology:measurement(rdt_tr_t10, radiologic_diagnostic_threshold, theater_ratio, 10, 0.15).

% Extraction over time (significant increase from liability shift):
narrative_ontology:measurement(rdt_ex_t0, radiologic_diagnostic_threshold, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(rdt_ex_t5, radiologic_diagnostic_threshold, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(rdt_ex_t10, radiologic_diagnostic_threshold, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint's primary function is to create a shared standard for interpreting medical data.
narrative_ontology:coordination_type(radiologic_diagnostic_threshold, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */