% ============================================================================
% CONSTRAINT STORY: slow_crisis_invisibility
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(slow_crisis_invisibility, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: slow_crisis_invisibility
 * human_readable: Generational Baseline Shift
 * domain: social/environmental
 * * SUMMARY:
 * This constraint describes the structural inability of short-term political 
 * systems to perceive or respond to crises with decay rates slower than a 
 * single biographical lifespan. It functions as a Snare for the future 
 * while appearing as a Mountain (unalterable nature) to the present.
 * * KEY AGENTS:
 * - Current Citizen: Subject (Powerless)
 * - Legacy Institution: Beneficiary (Institutional)
 * - Historical Modeler: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.75) because it consumes the optionality of future generations.
domain_priors:base_extractiveness(slow_crisis_invisibility, 0.75). 
domain_priors:suppression_score(slow_crisis_invisibility, 0.60).
domain_priors:theater_ratio(slow_crisis_invisibility, 0.20).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(slow_crisis_invisibility, extractiveness, 0.75).
narrative_ontology:constraint_metric(slow_crisis_invisibility, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(slow_crisis_invisibility, theater_ratio, 0.2).

% This is not a scaffold; it is an inherent cognitive/structural blindness.
% narrative_ontology:has_sunset_clause(slow_crisis_invisibility). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (MOUNTAIN)
% To an individual in a short time horizon, the slow crisis is just "how the world is."
constraint_indexing:constraint_classification(slow_crisis_invisibility, mountain, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutions use the invisibility to maintain current coordination and social 
% stability, avoiding the "cost" of radical transition.
constraint_indexing:constraint_classification(slow_crisis_invisibility, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% When viewed across historical time, the invisibility is a predatory trap 
% that extracts resources from the future to pay for the present.
constraint_indexing:constraint_classification(slow_crisis_invisibility, snare, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEM AUDITOR (TANGLED ROPE)
% Detects both the coordination function and the massive extraction.
constraint_indexing:constraint_classification(slow_crisis_invisibility, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(slow_crisis_invisibility, E), E >= 0.50,
    domain_priors:suppression_score(slow_crisis_invisibility, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(slow_crisis_invisibility_tests).

test(perspectival_gap) :-
    % Verify the "blindness": powerless see a Mountain, while auditors see a Snare.
    constraint_indexing:constraint_classification(slow_crisis_invisibility, mountain, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(slow_crisis_invisibility, snare, 
        context(agent_power(analytical), time_horizon(historical), _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(slow_crisis_invisibility, E),

    (E =< 0.05 ; E >= 0.46).

:- end_tests(slow_crisis_invisibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.75 extraction score represents the high degree of optionality taken 
 * from future agents who are 'trapped' by the decisions of the present.
 * * PERSPECTIVAL GAP:
 * The 'powerless' lacks the longitudinal data to see the trend, 
 * rendering the constraint a Mountain. The 'analytical' observer, using a 
 * historical time horizon, identifies the predatory nature (Snare).
 * * [RESOLVED MANDATROPHY]:
 * This resolution is achieved by the Tangled Rope classification at the 
0.75 threshold. It acknowledges that while extraction is high, the invisibility 
serves a temporary "coordination" function by preventing total social 
panic in the short term.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required because extraction (0.75) > 0.46
omega_variable(
    omega_baseline_shift,
    'Is the invisibility a cognitive hardware limit or a cultural software choice?',
    'Long-term immersive simulation of historical ecological baselines for cohorts.',
    'If cognitive: Mountain of Biology. If cultural: Snare of Education.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py
narrative_ontology:interval(slow_crisis_invisibility, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
