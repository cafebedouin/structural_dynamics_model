% ============================================================================
% CONSTRAINT STORY: capital_misallocation_spiral
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(capital_misallocation_spiral, []).

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
 * * constraint_id: capital_misallocation_spiral
 * human_readable: The Zombie Asset Loop
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where low interest rates or institutional mandates force capital 
 * into increasingly unproductive "zombie" assets. To prevent a crash, even 
 * more capital is allocated to prop up these failures, creating a recursive 
 * siphon. It functions as a Rope for short-term market stability but becomes 
 * a massive Snare for future generations who inherit a hollowed-out economy.
 * * KEY AGENTS:
 * - Emerging Innovator: Subject (Powerless)
 * - Central Financial Authority: Beneficiary (Institutional)
 * - Forensic Macroeconomist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) because the spiral siphons the species' real 
% productive potential to maintain the "value" of non-functional spreadsheets.
domain_priors:base_extractiveness(capital_misallocation_spiral, 0.87). 
domain_priors:suppression_score(capital_misallocation_spiral, 0.74). 
domain_priors:theater_ratio(capital_misallocation_spiral, 0.89). % Extreme theater: the "market" is a performative clearinghouse.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(capital_misallocation_spiral, extractiveness, 0.87).
narrative_ontology:constraint_metric(capital_misallocation_spiral, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(capital_misallocation_spiral, theater_ratio, 0.89).

% This is an entropic decay of price signals, not a temporary scaffold.
% narrative_ontology:has_sunset_clause(capital_misallocation_spiral). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the innovator, the spiral is a snare: capital is technically 
% abundant, but economically trapped in legacy propping, leaving them starved.
constraint_indexing:constraint_classification(capital_misallocation_spiral, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the intervention as a vital Rope—the only way to 
% coordinate market order and prevent systemic panic.
constraint_indexing:constraint_classification(capital_misallocation_spiral, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.89) > 0.70 triggers Piton: price signals have atrophied; 
% the system is an inert spike of logic propped up by performative valuation.
constraint_indexing:constraint_classification(capital_misallocation_spiral, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of high coordination intent (Ropes) 
% masking predatory, recursive extraction (Snare).
constraint_indexing:constraint_classification(capital_misallocation_spiral, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(capital_misallocation_spiral, E), E >= 0.50,
    domain_priors:suppression_score(capital_misallocation_spiral, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(capital_misallocation_spiral_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(capital_misallocation_spiral, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(capital_misallocation_spiral, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_detection) :-
    % Ensure high theater ratio (0.89) triggers Piton classification.
    constraint_indexing:constraint_classification(capital_misallocation_spiral, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.87) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(capital_misallocation_spiral, E),

    E > 0.70.

:- end_tests(capital_misallocation_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * "market" provides zero price-signal coordination relative to real utility.
 * 
 * * PERSPECTIVAL GAP:
 * The Emerging Innovator feels a Snare because they are effectively 
 * de-capitalized by institutional "stability" measures. The Financial 
 * Authority sees a Rope because the propping ensures no immediate collapse.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an 
 * analytical observer, the coordination is no longer functional 
 * (Theater 0.89); the system is an inert spike that siphons the 
 * future's wealth to pay for the past's mistakes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_signal_recovery,
    'Can price signals be restored without total systemic failure (Snare vs Mountain)?',
    'Tracking the delta between "shadow" market prices and institutional clearing prices.',
    'If signals recover: Snare of policy. If collapse is required: Mountain of Entropic Debt.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(capital_misallocation_spiral, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
