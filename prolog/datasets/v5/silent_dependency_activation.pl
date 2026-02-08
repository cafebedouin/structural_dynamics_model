% ============================================================================
% CONSTRAINT STORY: silent_dependency_activation
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(silent_dependency_activation, []).

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
 * * constraint_id: silent_dependency_activation
 * human_readable: The Invisible Supply Chain Trap
 * domain: technological/economic
 * * SUMMARY:
 * This constraint occurs when a system relies on a hidden, low-level component 
 * that remains invisible until a change in market conditions or policy 
 * activates it as a critical bottleneck. Once activated, it transforms from 
 * a background utility (Rope) into a high-extraction leverage point (Snare) 
 * for whoever controls the dependency.
 * * KEY AGENTS:
 * - Downstream Manufacturer: Subject (Powerless)
 * - IP/Patent Holder: Beneficiary (Institutional)
 * - Supply Chain Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.76) because the silent nature of the dependency 
% allows for the retroactive capture of entire industry margins.
domain_priors:base_extractiveness(silent_dependency_activation, 0.76).
domain_priors:suppression_score(silent_dependency_activation, 0.82). % Alternatives are often legally or technically impossible once locked in.
domain_priors:theater_ratio(silent_dependency_activation, 0.15).    % Low theater; the extraction is a direct mechanical/legal consequence.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(silent_dependency_activation, extractiveness, 0.76).
narrative_ontology:constraint_metric(silent_dependency_activation, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(silent_dependency_activation, theater_ratio, 0.15).

% This is not a scaffold; it is a structural vulnerability.
% narrative_ontology:has_sunset_clause(silent_dependency_activation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% For the manufacturer, the activation of the dependency is a pure extraction trap.
constraint_indexing:constraint_classification(silent_dependency_activation, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The IP holder views the dependency as a Rope for ensuring quality and coordination across the stack.
constraint_indexing:constraint_classification(silent_dependency_activation, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of standard-setting coordination vs predatory rent-seeking.
constraint_indexing:constraint_classification(silent_dependency_activation, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(silent_dependency_activation, E), E >= 0.50,
    domain_priors:suppression_score(silent_dependency_activation, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silent_dependency_activation_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Snare for the powerless but a Rope for the institution.
    constraint_indexing:constraint_classification(silent_dependency_activation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silent_dependency_activation, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(silent_dependency_activation, E),

    (E =< 0.05 -> true ; E >= 0.46).

:- end_tests(silent_dependency_activation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.76) reflects the 'Mandatrophy' state where 
 * the hidden nature of the dependency prevents agents from pricing in risk.
 * * PERSPECTIVAL GAP:
 * The Downstream Manufacturer feels a Snare because their sunk costs are 
 * effectively held hostage. The IP Holder sees a Rope as their component 
 * provides the "invisible" glue for the ecosystem.
 * * [RESOLVED MANDATROPHY]:
 * This is resolved by the Tangled Rope classification. While extraction is high, 
 * the component still performs a genuine coordination function—the system 
 * literally would not work without it—even if that function is asymmetrically 
 * monetized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_dependency_intent,
    'Was the dependency made silent by design (Snare) or by technological evolution (Mountain)?',
    'Forensic audit of technical standards documentation and private correspondence.',
    'If intentional: Snare. If evolutionary: Mountain of complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing.
narrative_ontology:interval(silent_dependency_activation, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
