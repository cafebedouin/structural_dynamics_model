% ============================================================================
% CONSTRAINT STORY: structural_extraction_without_actor
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(structural_extraction_without_actor, []).

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
 * * constraint_id: structural_extraction_without_actor
 * human_readable: The Inertial Rent Siphon
 * domain: economic/social
 * * SUMMARY:
 * This constraint represents a legacy fee-structure or bureaucratic 
 * requirement where the original "service provider" or "beneficiary" 
 * has long since vanished or automated, yet the extraction continues 
 * via institutional inertia. It is an "actorless" siphon.
 * * KEY AGENTS:
 * - Current Payer: Subject (Powerless)
 * - Legacy System: Beneficiary (Institutional - though the 'human' beneficiary is absent)
 * - Systems Archeologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as it is pure rent with zero service return.
domain_priors:base_extractiveness(structural_extraction_without_actor, 0.78). 
domain_priors:suppression_score(structural_extraction_without_actor, 0.65).
domain_priors:theater_ratio(structural_extraction_without_actor, 0.85). % High theater/inertia.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(structural_extraction_without_actor, extractiveness, 0.78).
narrative_ontology:constraint_metric(structural_extraction_without_actor, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(structural_extraction_without_actor, theater_ratio, 0.85).

% This has no sunset clause, contributing to its "Piton" nature.
% narrative_ontology:has_sunset_clause(structural_extraction_without_actor). 

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The payer is trapped in a loop where they pay for a non-existent utility.
constraint_indexing:constraint_classification(structural_extraction_without_actor, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institutional code views this as a "coordination" prerequisite for system solvency.
constraint_indexing:constraint_classification(structural_extraction_without_actor, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Because theater_ratio (0.85) > 0.70, the auditor sees it as a dead, inertial spike.
constraint_indexing:constraint_classification(structural_extraction_without_actor, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(structural_extraction_without_actor, TR), TR > 0.70.

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature of the extraction code vs the coordination legacy.
constraint_indexing:constraint_classification(structural_extraction_without_actor, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(structural_extraction_without_actor, E), E >= 0.50,
    domain_priors:suppression_score(structural_extraction_without_actor, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(structural_extraction_without_actor_tests).

test(perspectival_gap) :-
    % Verify the Snare vs Rope conflict.
    constraint_indexing:constraint_classification(structural_extraction_without_actor, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(structural_extraction_without_actor, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Verify the auditor detects the Piton via theater_ratio.
    constraint_indexing:constraint_classification(structural_extraction_without_actor, piton, 
        context(agent_power(analytical), _, _, _)).

:- end_tests(structural_extraction_without_actor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score of 0.78 triggers mandatory Mandatrophy resolution. 
 * The system is effectively actorless—the 'Beneficiary' is a legacy algorithm.
 * * PERSPECTIVAL GAP:
 * The Subject experiences a Snare (wealth transfer without benefit). The 
 * Institution experiences a Rope (a necessary entry in the ledger).
 * * [RESOLVED MANDATROPHY]:
 * This is resolved by identifying the constraint as a Piton for the analytical 
 * index. The extraction persists not because of an active predator, but 
 * because the cost of removing the "Piton" exceeds the perceived benefit 
 * for the institutional actors.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for base_extractiveness > 0.46
omega_variable(
    omega_actorless_leak,
    'Is there a hidden beneficiary (Snare) or is the extraction truly an entropic byproduct (Mountain)?',
    'Full-stack ledger forensic audit of legacy escrow accounts.',
    'If hidden actor: Snare. If entropic: Mountain of the system.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for structural_linter.py
narrative_ontology:interval(structural_extraction_without_actor, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
