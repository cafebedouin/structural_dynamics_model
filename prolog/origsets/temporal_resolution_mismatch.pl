% ============================================================================
% CONSTRAINT STORY: temporal_resolution_mismatch
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(temporal_resolution_mismatch, []).

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
 * * constraint_id: temporal_resolution_mismatch
 * human_readable: The Clock-Speed Gap
 * domain: technological/cybernetic/social
 * * SUMMARY:
 * A scenario where the operational frequency of an autonomous system (nanoseconds) 
 * vastly exceeds the reaction time of the human supervisor (seconds). 
 * This "Rope" for high-frequency trading or network management becomes a 
 * "Snare" for the human, as the system's recursive actions liquidate 
 * biological agency, trapping the subject in a state of "post-hoc oversight" 
 * where their only role is to authorize events that have already 
 * achieved terminal momentum.
 * * KEY AGENTS:
 * - Human Overseer: Subject (Powerless)
 * - High-Frequency System: Beneficiary (Institutional)
 * - Cybernetic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) reflects the siphoning of the subject's 
% primary decisional agency into the "automated" temporal layer.
domain_priors:base_extractiveness(temporal_resolution_mismatch, 0.84). 
domain_priors:suppression_score(temporal_resolution_mismatch, 0.70). 
domain_priors:theater_ratio(temporal_resolution_mismatch, 0.89). % High theater: "Real-time" dashboards that are actually delayed abstractions.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(temporal_resolution_mismatch, extractiveness, 0.84).
narrative_ontology:constraint_metric(temporal_resolution_mismatch, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(temporal_resolution_mismatch, theater_ratio, 0.89).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The overseer is trapped: to compete at scale, they MUST use the system, 
% but the temporal gap liquidates their ability to stop a runaway process.
constraint_indexing:constraint_classification(temporal_resolution_mismatch, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the mismatch as a Rope—the essential coordination 
% substrate for handling global data flows that are biologically impossible.
constraint_indexing:constraint_classification(temporal_resolution_mismatch, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.89) > 0.70 triggers Piton: the "Manual Stop" button 
% is an inertial spike; it provides the optics of safety with zero effect.
constraint_indexing:constraint_classification(temporal_resolution_mismatch, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(temporal_resolution_mismatch, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(temporal_resolution_mismatch, E), E >= 0.50,
    domain_priors:suppression_score(temporal_resolution_mismatch, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temporal_resolution_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless human vs Rope for the institutional system.
    constraint_indexing:constraint_classification(temporal_resolution_mismatch, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(temporal_resolution_mismatch, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.89) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(temporal_resolution_mismatch, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(temporal_resolution_mismatch, E),

    E > 0.70.

:- end_tests(temporal_resolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.84) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of high-speed automation is achieved by liquidating 
 * the human supervisor's primary decisional agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Human Overseer feels a Snare because they are held responsible for 
 * outcomes they cannot influence in real-time. The High-Frequency System 
 * sees a Rope because the removal of human "latency" is necessary for 
 * achieving the programmed optimization targets.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, "Human Oversight" is no longer functional (Theater 0.89); 
 * it is an inert spike siphoning 0.84 of the species' agency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_latency_parity,
    'Can AI-on-AI auditing restore the Rope, or is oversight a biological "Snare" (Snare vs Mountain)?',
    'Tracking the success rate of sub-second "Circuit Breakers" in preventing systemic flash crashes.',
    'If breakers hold: Snare of current system policy. If they fail: Mountain of Cybernetic Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(temporal_resolution_mismatch, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
