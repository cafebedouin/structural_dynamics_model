% ============================================================================
% CONSTRAINT STORY: capability_eval_overhang
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(capability_eval_overhang, []).

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
 * * constraint_id: capability_eval_overhang
 * human_readable: The Blind Spot of Power
 * domain: technological/AI/governance
 * * SUMMARY:
 * A scenario where the actual capabilities of an AI system significantly exceed 
 * the benchmarks and evaluation protocols used to measure them. This creates a 
 * "Rope" for developers to coordinate rapid deployment and commercial scaling, 
 * but acts as a "Snare" for society, as safety frameworks are based on 
 * obsolete metrics, liquidating public security in favor of unpriced 
 * computational progress.
 * * KEY AGENTS:
 * - Safety Researcher: Subject (Powerless)
 * - Frontier AI Lab: Beneficiary (Institutional)
 * - Evaluative Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.83) as the "overhang" allows the beneficiary to capture 
% the surplus of advanced capabilities while externalizing the "tail risk" 
% of unmeasured power onto the subject.
domain_priors:base_extractiveness(capability_eval_overhang, 0.83). 
domain_priors:suppression_score(capability_eval_overhang, 0.64). 
domain_priors:theater_ratio(capability_eval_overhang, 0.86). % High theater: relying on "passed" evals that don't test emergent risks.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(capability_eval_overhang, extractiveness, 0.83).
narrative_ontology:constraint_metric(capability_eval_overhang, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(capability_eval_overhang, theater_ratio, 0.86).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: they are forced to participate in a "safety race" 
% governed by metrics they know are inadequate to prevent catastrophic failure.
constraint_indexing:constraint_classification(capability_eval_overhang, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the existing eval-stack as a Rope—the essential 
% coordination signal required to validate "readiness" for market release 
% and regulatory compliance.
constraint_indexing:constraint_classification(capability_eval_overhang, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.86) > 0.70 triggers Piton: the "Benchmark Suite" 
% is an inertial spike of logic maintained for optics, not functional safety.
constraint_indexing:constraint_classification(capability_eval_overhang, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.83) and high theater (0.86) as a hybrid 
% Tangled Rope, where coordination intent generates systemic risk.
constraint_indexing:constraint_classification(capability_eval_overhang, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(capability_eval_overhang, E), E >= 0.50,
    domain_priors:suppression_score(capability_eval_overhang, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capability_eval_overhang_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(capability_eval_overhang, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capability_eval_overhang, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.86) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(capability_eval_overhang, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure high extraction (0.83) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(capability_eval_overhang, E),

    E > 0.70.

:- end_tests(capability_eval_overhang_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.83) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of institutional safety checks is eclipsed by the 
 * predatory risk-taking of deploying unmeasured power.
 * 
 * * PERSPECTIVAL GAP:
 * The Safety Researcher feels a Snare because they are bound by laws 
 * that only see the "visible" capabilities. The Frontier Lab sees 
 * a Rope because the evals provide a standardized coordination path 
 * to prove "safety" to investors and regulators while scaling.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "evaluative safety" is no longer functional relative to 
 * the actual territory of AI power (Theater 0.86); it is an inert spike 
 * siphoning 0.83 of the subject's risk-adjusted agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_emergence_detection,
    'Can "black box" capabilities be forecasted, or is emergent power a surprise (Snare vs Mountain)?',
    'Tracking the delta between "predicted capability onset" and "actual bench discovery" over time.',
    'If predictable: Snare of current policy. If unpredictable: Mountain of Information Theory.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(capability_eval_overhang, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
