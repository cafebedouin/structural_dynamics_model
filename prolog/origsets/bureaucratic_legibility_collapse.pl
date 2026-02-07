% ============================================================================
% CONSTRAINT STORY: bureaucratic_legibility_collapse
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(bureaucratic_legibility_collapse, []).

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
 * * constraint_id: bureaucratic_legibility_collapse
 * human_readable: The Administrative Whiteout
 * domain: political/organizational/informational
 * * SUMMARY:
 * A scenario where the metrics used by an institution to "see" and manage its 
 * domain become so decoupled from reality that the institution's actions 
 * produce the opposite of their intended effects. This "Rope" for 
 * large-scale governance becomes a "Snare" as the subject's reality is 
 * erased by the system's map, liquidating their ability to provide 
 * meaningful feedback or exit the failing administrative logic.
 * * KEY AGENTS:
 * - Field Practitioner: Subject (Powerless)
 * - Central Planning Bureau: Beneficiary (Institutional)
 * - Information Systems Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.86) as the collapse of legibility siphons the subject's 
% real-world effort into the maintenance of non-functional reporting 
% structures that no longer track the actual territory.
domain_priors:base_extractiveness(bureaucratic_legibility_collapse, 0.86). 
domain_priors:suppression_score(bureaucratic_legibility_collapse, 0.74). 
domain_priors:theater_ratio(bureaucratic_legibility_collapse, 0.92). % Extreme theater: meticulous metrics masking zero situational awareness.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, extractiveness, 0.86).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(bureaucratic_legibility_collapse, theater_ratio, 0.92).

% This represents a terminal entropic state of information-dense hierarchies.
% narrative_ontology:has_sunset_clause(bureaucratic_legibility_collapse). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: their actual problems are illegible to the 
% system, and they are punished for deviations from the "official" (false) data.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the metrics as a Rope—the only way to coordinate 
% behavior across a vast and diverse population from a centralized point.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "governance" framework 
% is an inert, performative artifact that no longer steers the system.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.86) and high theater (0.92) as a hybrid 
% Tangled Rope, where coordination intent generates systemic blindness.
constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(bureaucratic_legibility_collapse, E), E >= 0.50,
    domain_priors:suppression_score(bureaucratic_legibility_collapse, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_legibility_collapse_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional planner.
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(bureaucratic_legibility_collapse, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_mandatrophy) :-
    % Ensure extraction (0.86) triggers mandatory v3.4 resolution logic.
    domain_priors:base_extractiveness(bureaucratic_legibility_collapse, E),

    E > 0.70.

:- end_tests(bureaucratic_legibility_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.86) reflects a "Mandatrophy" state where the 
 * "coordination" is actually a parasitic liquidation of the territory's reality.
 * 
 * * PERSPECTIVAL GAP:
 * The Field Practitioner feels a Snare because they must lie on reports 
 * to make their work "count." The Central Bureau sees a Rope 
 * because the metrics are the only thing that allows for the coordination 
 * of large-scale budgets and personnel.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an 
 * analytical observer, the "legibility" is no longer functional 
 * (Theater 0.92); the system is an inert spike siphoning 0.86 of the 
 * subject's agency to feed an imaginary model.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_reality_reconnection,
    'Can decentralized feedback loops restore legibility, or is hierarchy an irreducible barrier (Snare vs Mountain)?',
    'Tracking the delta between local practitioner reports and central bureau metrics after a policy shock.',
    'If delta shrinks: Snare of current design. If delta grows: Mountain of Informational Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py.
narrative_ontology:interval(bureaucratic_legibility_collapse, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
