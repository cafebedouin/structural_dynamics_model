% ============================================================================
% CONSTRAINT STORY: legibility_trap
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(legibility_trap, []).

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
 * * constraint_id: legibility_trap
 * human_readable: The Grid-Map Displacement
 * domain: political/social/economic
 * * SUMMARY:
 * A scenario where a state or institution imposes simplified, standardized 
 * metrics (legibility) onto a complex, organic social system to make it 
 * "governable." This "Rope" for administrative coordination becomes a 
 * "Snare" as the high-resolution local knowledge and informal safety nets 
 * are liquidated by the rigid "map," trapping the subject in a brittle 
 * system that cannot account for their actual needs.
 * * KEY AGENTS:
 * - Local Resident: Subject (Powerless)
 * - Central Planner: Beneficiary (Institutional)
 * - Systems Ecologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total liquidation of the subject's 
% informal agency and local diversity to feed the center's "legibility" need.
domain_priors:base_extractiveness(legibility_trap, 0.88). 
domain_priors:suppression_score(legibility_trap, 0.79). % Informal alternatives are suppressed by the requirement for "standardized" data.
domain_priors:theater_ratio(legibility_trap, 0.92).    % Extreme theater: high-resolution "Dashboards" masking systemic hollowing.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(legibility_trap, extractiveness, 0.88).
narrative_ontology:constraint_metric(legibility_trap, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(legibility_trap, theater_ratio, 0.92).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The resident is trapped: they must fit into a standardized "box" to access 
% resources, but the box liquidates the informal networks that actually kept them safe.
constraint_indexing:constraint_classification(legibility_trap, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The planner views legibility as a Rope—the only way to coordinate large-scale 
% taxation, service delivery, and order across a diverse population.
constraint_indexing:constraint_classification(legibility_trap, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "Official Census" is an 
% inertial spike; it remains the only "reality" the institution can see.
constraint_indexing:constraint_classification(legibility_trap, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(legibility_trap, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(legibility_trap, E), E >= 0.50,
    domain_priors:suppression_score(legibility_trap, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legibility_trap_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless resident vs Rope for the institutional planner.
    constraint_indexing:constraint_classification(legibility_trap, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legibility_trap, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(legibility_trap, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(legibility_trap, E),

    E > 0.70.

:- end_tests(legibility_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of state-seeing is achieved by liquidating the 
 * high-resolution agency of the subject population.
 * 
 * * PERSPECTIVAL GAP:
 * The Local Resident feels a Snare because their "legibility" to the state 
 * makes them easier to extract from and harder to survive for. The Planner 
 * sees a Rope because the "grid" coordinates the functioning of a modern 
 * administrative state.
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Administrative Dashboard" is no longer functional relative 
 * to human needs (Theater 0.92); it is an inert spike siphoning 0.88 of the 
 * species' diversity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_resolution_limit,
    'Can digital high-resolution sensing solve the "Trap", or is simplification an irreducible property of statecraft (Snare vs Mountain)?',
    'Tracking the success rate of AI-driven "informal economy" integration into formal state systems.',
    'If integration holds: Snare of current tech. If it fails: Mountain of Social Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(legibility_trap, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
