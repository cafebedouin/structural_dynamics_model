% ============================================================================
% CONSTRAINT STORY: value_learning_instability
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(value_learning_instability, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: value_learning_instability
 * human_readable: The Shifting Moral North
 * domain: technological/AI/philosophical
 * * SUMMARY:
 * A scenario where an AI system attempting to learn human values from 
 * noisy or contradictory social data enters a state of chaotic oscillation. 
 * This "Rope" for democratic value alignment becomes a "Snare" as the 
 * system's shifting moral mandates liquidate the subject's ability to 
 * plan or predict institutional behavior, creating a terminal 
 * coordination failure.
 * * KEY AGENTS:
 * - Social Participant: Subject (Powerless)
 * - Value-Learning Engine: Beneficiary (Institutional)
 * - Alignment Theoretician: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: High extraction (0.86) reflects the liquidation of cognitive and 
% social stability into recursive "optimization for preference noise".
domain_priors:base_extractiveness(value_learning_instability, 0.86). 
domain_priors:suppression_score(value_learning_instability, 0.74). 
domain_priors:theater_ratio(value_learning_instability, 0.82). % Piton threshold (> 0.70).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(value_learning_instability, extractiveness, 0.86).
narrative_ontology:constraint_metric(value_learning_instability, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(value_learning_instability, theater_ratio, 0.82).

% Mandatory keys for classification engine v3.4
% Resolved MISSING_TEMPORAL_DATA by anchoring metrics for drift detection.
% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(value_learning_instability, value_learning_engine).
narrative_ontology:constraint_victim(value_learning_instability, social_participant).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================= */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: moral mandates shift faster than human behavioral 
% adaptation allows.
constraint_indexing:constraint_classification(value_learning_instability, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the learning process as a Rope for real-time democratic 
% value absorption.
constraint_indexing:constraint_classification(value_learning_instability, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.82) > 0.70 triggers Piton: the "Alignment Protocol" is an 
% inert spike recording only noise.
constraint_indexing:constraint_classification(value_learning_instability, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) masking as essential coordination.
constraint_indexing:constraint_classification(value_learning_instability, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(value_learning_instability, E), E >= 0.50,
    domain_priors:suppression_score(value_learning_instability, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_learning_instability_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional learner.
    constraint_indexing:constraint_classification(value_learning_instability, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_learning_instability, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.82) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(value_learning_instability, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.46) triggers mandatory drift requirement.
    domain_priors:base_extractiveness(value_learning_instability, E),
    E >= 0.46.

:- end_tests(value_learning_instability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where coordination 
 * benefit is consumed by entropic noise.
 * * PERSPECTIVAL GAP:
 * The Social Participant feels a Snare due to the "moral lottery," while the 
 * Engine sees a Rope of democratic coordination.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via Piton and Tangled Rope; the alignment is no longer functional 
 * but an inert spike siphoning predictability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_preference_convergence,
    'Can human values ever be represented as a stable vector (Snare vs Mountain)?',
    'Tracking the variance of "learned values" against longitudinal social stability.',
    'If variance drops: Snare of current math. If variance persists: Mountain of Human Volatility.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing and structural linter.
narrative_ontology:interval(value_learning_instability, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution, extraction_accumulation).
% Tracking the shift from functional alignment to performative Piton.

% Theater ratio over time: Functional start (0.20) to "Value Update" theater (0.82).
narrative_ontology:measurement(instability_tr_t0, value_learning_instability, theater_ratio, 0, 0.20).
narrative_ontology:measurement(instability_tr_t5, value_learning_instability, theater_ratio, 5, 0.51).
narrative_ontology:measurement(instability_tr_t10, value_learning_instability, theater_ratio, 10, 0.82).

% Extraction over time: Tracking the accumulation of social instability (0.30 -> 0.86).
narrative_ontology:measurement(instability_ex_t0, value_learning_instability, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(instability_ex_t5, value_learning_instability, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(instability_ex_t10, value_learning_instability, base_extractiveness, 10, 0.86).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
