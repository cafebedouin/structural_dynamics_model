% ============================================================================
% CONSTRAINT STORY: awareness_without_leverage
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(awareness_without_leverage, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Updated for v3.4) ---
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
 * * constraint_id: awareness_without_leverage
 * human_readable: The Cassandra Paradox
 * domain: social/political
 * * SUMMARY:
 * This constraint occurs when an agent possesses high-fidelity information 
 * regarding a systemic failure but lacks any structural mechanism to 
 * influence the outcome. This asymmetry turns the information itself into 
 * a psychological Snare rather than a tool for coordination.
 * * KEY AGENTS:
 * - Cassandra: Subject (Powerless)
 * - Inertial System: Beneficiary (Institutional)
 * - Power Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (REVISED)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(awareness_without_leverage, 0.73).
domain_priors:suppression_score(awareness_without_leverage, 0.65).
domain_priors:theater_ratio(awareness_without_leverage, 0.40).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(awareness_without_leverage, extractiveness, 0.73).
narrative_ontology:constraint_metric(awareness_without_leverage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(awareness_without_leverage, theater_ratio, 0.4).

% Constraint metric facts — primary keys used by the classification engine v3.4.
% Beneficiaries & Victims (Required for extraction > 0.46)
narrative_ontology:constraint_beneficiary(awareness_without_leverage, inertial_system).
narrative_ontology:constraint_victim(awareness_without_leverage, cassandra).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped by their own knowledge, unable to act or exit.
constraint_indexing:constraint_classification(awareness_without_leverage, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the distribution of information without leverage as a 
% 'Rope'—a way to maintain the appearance of transparency without the risk 
% of actual structural disruption.
constraint_indexing:constraint_classification(awareness_without_leverage, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% High extraction (0.73) and suppression (0.65) trigger the Tangled Rope hybrid.
constraint_indexing:constraint_classification(awareness_without_leverage, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(awareness_without_leverage, E), E >= 0.50,
    domain_priors:suppression_score(awareness_without_leverage, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(awareness_without_leverage_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject and Rope for the institution.
    constraint_indexing:constraint_classification(awareness_without_leverage, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(awareness_without_leverage, rope, 
        context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction meets the high-extraction core limits.
    domain_priors:base_extractiveness(awareness_without_leverage, E),
    (E =< 0.05 ; E >= 0.46).

:- end_tests(awareness_without_leverage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.73) reflects the 'Mandatrophy' threshold where 
 * awareness increases without a corresponding increase in agency.
 * * PERSPECTIVAL GAP:
 * The Subject experiences a Snare (knowledge without power); the Institution 
 * experiences a Rope (transparency as a stability mechanism).
 * * [RESOLVED MANDATROPHY]:
 * Resolved by the 'Tangled Rope' observer perspective. The system provides 
 * coordination via information flow, but that flow is asymmetrically decoupled 
 * from leverage, creating a predatory psychological state.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_leverage_bottleneck,
    'Is the lack of leverage a design feature (Snare) or a scaling limit (Mountain)?',
    'Implementation of decentralized quadratic voting for informed cohorts.',
    'If leverage increases: Snare of architecture. If stalemate remains: Mountain.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(awareness_without_leverage, 0, 10). 

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio: Modeling the drift of performative transparency (0.15) 
% toward a stable but non-functional state (0.40).
narrative_ontology:measurement(awl_tr_t0, awareness_without_leverage, theater_ratio, 0, 0.15).
narrative_ontology:measurement(awl_tr_t5, awareness_without_leverage, theater_ratio, 5, 0.28).
narrative_ontology:measurement(awl_tr_t10, awareness_without_leverage, theater_ratio, 10, 0.40).

% Extraction: Progressive accumulation of the "psychological tax" as 
% awareness of systemic failure grows while leverage remains suppressed.
narrative_ontology:measurement(awl_ex_t0, awareness_without_leverage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(awl_ex_t5, awareness_without_leverage, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(awl_ex_t10, awareness_without_leverage, base_extractiveness, 10, 0.73).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
