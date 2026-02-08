% ============================================================================
% CONSTRAINT STORY: container_capacity_mismatch
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(container_capacity_mismatch, []).

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
 * * constraint_id: container_capacity_mismatch
 * human_readable: The Volume-Infrastructure Paradox
 * domain: logistics/physical_infrastructure/economic
 * * SUMMARY:
 * A scenario where the volume of commodities or data produced by a 
 * hyper-efficient "source" (Rope) vastly exceeds the physical or logical 
 * capacity of the "containers" or "conduits" designed to transport them. 
 * This mismatch acts as a "Snare" for the small-scale operator, whose 
 * inventory agency is liquidated as they are forced to discard surplus or 
 * pay exorbitant "congestion rent" to access the bottlenecked infrastructure, 
 * trapping them in a state of terminal unprofitability despite high productivity.
 *
 * * KEY AGENTS:
 * - Independent Producer: Subject (Powerless)
 * - Infrastructure Gatekeeper: Beneficiary (Institutional)
 * - Supply Chain Forensic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) reflects the systematic liquidation of the producer's 
% surplus through artificial or physical scarcity rents.
domain_priors:base_extractiveness(container_capacity_mismatch, 0.85). 
domain_priors:suppression_score(container_capacity_mismatch, 0.74). % Alternative transport methods are suppressed by regulatory or physical monopoly.
domain_priors:theater_ratio(container_capacity_mismatch, 0.82).    % High theater: "Efficiency optimization" software that masks the physical bottleneck.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(container_capacity_mismatch, extractiveness, 0.85).
narrative_ontology:constraint_metric(container_capacity_mismatch, suppression_requirement, 0.74).
narrative_ontology:constraint_metric(container_capacity_mismatch, theater_ratio, 0.82).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The producer is trapped: they produce more value than ever, but the 
% lack of "container" space liquidates their ability to realize that value.
constraint_indexing:constraint_classification(container_capacity_mismatch, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The gatekeeper views the mismatch as a Rope—the essential coordination 
% substrate for maintaining high prices and ensuring "systemic stability" 
% through throttled supply.
constraint_indexing:constraint_classification(container_capacity_mismatch, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.82) > 0.70 triggers Piton: the "Congestion Management" 
% protocol is an inertial spike; it signals fairness while enabling extraction.
constraint_indexing:constraint_classification(container_capacity_mismatch, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.85) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(container_capacity_mismatch, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(container_capacity_mismatch, E), E >= 0.50,
    domain_priors:suppression_score(container_capacity_mismatch, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(container_capacity_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless producer vs Rope for the institutional gatekeeper.
    constraint_indexing:constraint_classification(container_capacity_mismatch, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(container_capacity_mismatch, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.82) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(container_capacity_mismatch, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(container_capacity_mismatch, E),

    E > 0.70.

:- end_tests(container_capacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of organized logistics is achieved by liquidating 
 * the subject's primary productive agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Independent Producer feels a Snare because their success creates 
 * the very volume that destroys their margins. The Gatekeeper sees a Rope 
 * because the bottleneck coordinates a lucrative, legible scarcity.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Logistics Optimization" suite is no longer functional 
 * (Theater 0.82); it is an inert spike siphoning 0.85 of the producer's surplus. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_infrastructure_elasticity,
    'Can digital modularity (e.g., packet switching) be applied to physical atoms, or is the Snare a Mountain of physics (Snare vs Mountain)?',
    'Tracking the delta between production growth and transit-infrastructure throughput over 20 years.',
    'If throughput plateaus: Mountain of Physical Geometry. If it scales: Snare of current design.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(container_capacity_mismatch, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
