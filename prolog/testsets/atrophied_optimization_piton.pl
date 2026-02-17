% ============================================================================
% CONSTRAINT STORY: atrophied_optimization_piton
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_atrophied_optimization, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: atrophied_optimization_piton
 * human_readable: The Ghost in the Habit
 * domain: technological/social
 * * SUMMARY:
 * A state where an AI optimization loop has been deactivated, but the 
 * human workflows, KPIs, and behavioral nudges it created remain mandatory. 
 * It is a Piton: a structural fossil 
 * that extracts effort for a goal that is no longer being calculated. 
 *
 * * KEY AGENTS:
 * - The Habitual User: Subject (Powerless) - Following "ghost" recommendations.
 * - The Legacy Manager: Beneficiary (Institutional) - Maintaining the status quo.
 * - The Systems Archaeologist: Auditor (Analytical) - Mapping the dead logic.
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(atrophied_optimization_piton, 0.48). 
domain_priors:suppression_score(atrophied_optimization_piton, 0.80).   
domain_priors:theater_ratio(atrophied_optimization_piton, 0.95).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(atrophied_optimization_piton, extractiveness, 0.48).
narrative_ontology:constraint_metric(atrophied_optimization_piton, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(atrophied_optimization_piton, theater_ratio, 0.95).

% Constraint classification claim
narrative_ontology:constraint_claim(atrophied_optimization_piton, piton).
narrative_ontology:human_readable(atrophied_optimization_piton, "The Ghost in the Habit").

% Mandatory keys for classification engine v3.4
% Binary flags
domain_priors:requires_active_enforcement(atrophied_optimization_piton).

% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(atrophied_optimization_piton, legacy_manager).
narrative_ontology:constraint_victim(atrophied_optimization_piton, habitual_user).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user is trapped in behavioral patterns that no longer serve even the 
% original machine goal, let alone their own.
constraint_indexing:constraint_classification(atrophied_optimization_piton, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE INSTITUTION (ROPE)
% To the management, the rituals are a Rope—they provide the "predictability" 
% and "order" required for institutional stability.
constraint_indexing:constraint_classification(atrophied_optimization_piton, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% Detection of "Piton" status: The extreme theater_ratio (0.95) reveals a 
% constraint that has outlived its technical engine.
constraint_indexing:constraint_classification(atrophied_optimization_piton, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(global))) :-
    domain_priors:theater_ratio(atrophied_optimization_piton, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(atrophied_piton_tests).

test(piton_identity_check) :-
    % Verify the auditor identifies the Piton based on the 0.70 theater threshold.
    constraint_indexing:constraint_classification(atrophied_optimization_piton, piton, context(agent_power(analytical), _, _, _)).

test(mandatrophy_check) :-
    % Extraction > 0.46 triggers Omega validation.
    domain_priors:base_extractiveness(atrophied_optimization_piton, E), E > 0.46.

:- end_tests(atrophied_piton_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The theater_ratio (0.95) is the highest possible value, as the "engine" 
 * of the constraint is literally gone.
 * * [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by identifying that the "coordination" is now 
 * purely social/ritualistic. By labeling it a Piton, we distinguish it from 
 * a functional Rope or an unavoidable Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ritual_utility,
    'Do the "ghost" habits provide a hidden coordination benefit (Rope) that justifies their extraction?',
    'Productivity audit comparing "Ghost Habit" cohorts vs. "Clean Slate" cohorts.',
    'If utility exists: It is a low-tension Rope; If not: It is a pure Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(atrophied_optimization_piton, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Rising from functional AI-driven coordination (0.15) 
% to extreme "Ghost Habit" theater (0.95) following deactivation.
narrative_ontology:measurement(piton_tr_t0, atrophied_optimization_piton, theater_ratio, 0, 0.15).
narrative_ontology:measurement(piton_tr_t5, atrophied_optimization_piton, theater_ratio, 5, 0.52).
narrative_ontology:measurement(piton_tr_t10, atrophied_optimization_piton, theater_ratio, 10, 0.95).

% Extraction: Tracking the intensification of "Meaningless Labor" as 
% workflows harden into mandatory but non-functional fossils.
narrative_ontology:measurement(piton_ex_t0, atrophied_optimization_piton, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(piton_ex_t5, atrophied_optimization_piton, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(piton_ex_t10, atrophied_optimization_piton, base_extractiveness, 10, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
