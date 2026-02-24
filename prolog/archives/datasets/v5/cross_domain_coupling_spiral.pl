% ============================================================================
% CONSTRAINT STORY: cross_domain_coupling_spiral
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(cross_domain_coupling_spiral, []).

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
 * * constraint_id: cross_domain_coupling_spiral
 * human_readable: The Entangled Dependency Vortex
 * domain: technological/economic/cybernetic
 * * SUMMARY:
 * A scenario where increasing efficiency is sought by tightly coupling 
 * independent domains (e.g., energy grids, financial markets, and digital 
 * identity systems). While this "Rope" for global coordination creates 
 * massive systemic throughput, it becomes a "Snare" for the subject as 
 * failures in one domain spiral instantly into others, liquidating the 
 * subject's ability to isolate risks or maintain functional autonomy in 
 * any single sector.
 * * KEY AGENTS:
 * - Local Infrastructure Manager: Subject (Powerless)
 * - Integrated Platform Architect: Beneficiary (Institutional)
 * - Systemic Contagion Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total liquidation of domain-specific 
% resilience to feed the institutional need for integrated efficiency.
domain_priors:base_extractiveness(cross_domain_coupling_spiral, 0.88). 
domain_priors:suppression_score(cross_domain_coupling_spiral, 0.79). % "Decoupled" alternatives are suppressed as inefficient or obsolete.
domain_priors:theater_ratio(cross_domain_coupling_spiral, 0.91).    % High theater: "Resilience Certifications" masking the reality of cascading vulnerability.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cross_domain_coupling_spiral, extractiveness, 0.88).
narrative_ontology:constraint_metric(cross_domain_coupling_spiral, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(cross_domain_coupling_spiral, theater_ratio, 0.91).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The manager is trapped: a software bug in a remote data center liquidates 
% their local energy grid's stability, leaving them with zero exit options.
constraint_indexing:constraint_classification(cross_domain_coupling_spiral, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the coupling as a Rope—the essential coordination 
% substrate for achieving "Total System Optimization" across global markets.
constraint_indexing:constraint_classification(cross_domain_coupling_spiral, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Failover Strategy" 
% is an inertial spike; it performatively charts a recovery path that 
% cannot withstand cross-domain contagion.
constraint_indexing:constraint_classification(cross_domain_coupling_spiral, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) masking as functional scaling (Rope).
constraint_indexing:constraint_classification(cross_domain_coupling_spiral, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(cross_domain_coupling_spiral, E), E >= 0.50,
    domain_priors:suppression_score(cross_domain_coupling_spiral, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_domain_spiral_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless manager vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(cross_domain_coupling_spiral, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_domain_coupling_spiral, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(cross_domain_coupling_spiral, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(cross_domain_coupling_spiral, E),

    E > 0.70.

:- end_tests(cross_domain_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of tight coupling is achieved by liquidating the 
 * subject's primary risk-management agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Local Infrastructure Manager feels a Snare because they are forced 
 * into a dependency where they suffer the consequences of failures they 
 * did not cause. The Architect sees a Rope because the coupling coordinates 
 * a level of efficiency and liquidity impossible in a decoupled state.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Systemic Stability Dashboard" is no longer functional 
 * (Theater 0.91); it is an inert spike siphoning 0.88 of the species' 
 * collective resilience agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_coupling_irreversibility,
    'Can "Circuit Breakers" restore the Rope, or is contagion a physical law of networks (Snare vs Mountain)?',
    'Tracking the propagation velocity of financial shocks into energy-grid availability in 2026.',
    'If breakers hold: Snare of current system design. If they fail: Mountain of Information Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cross_domain_coupling_spiral, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
