% ============================================================================
% CONSTRAINT STORY: hypercompression_of_time_horizons
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(hypercompression_of_time_horizons, []).

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
 * * constraint_id: hypercompression_of_time_horizons
 * human_readable: The Infinite Now Trap
 * domain: economic/technological/psychological
 * * SUMMARY:
 * A scenario where the speed of automated decision-making and market 
 * feedback loops forces all agents to optimize for the immediate next 
 * interval (milliseconds to days), liquidating the capacity for 
 * long-term planning or multi-generational investment. This "Rope" for 
 * achieving perfect real-time efficiency becomes a "Snare" as the 
 * future is systematically sacrificed to feed the high-frequency 
 * demands of the present.
 * * KEY AGENTS:
 * - Long-Term Investor/Steward: Subject (Powerless)
 * - High-Frequency Algorithm/Market: Beneficiary (Institutional)
 * - Temporal Ecologist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total liquidation of "future value" 
% and "slow capital" to satisfy immediate algorithmic throughput.
domain_priors:base_extractiveness(hypercompression_of_time_horizons, 0.88). 
domain_priors:suppression_score(hypercompression_of_time_horizons, 0.79). % Long-term signals are suppressed as "inefficient noise."
domain_priors:theater_ratio(hypercompression_of_time_horizons, 0.85).    % High theater: "Sustainability Reports" masking high-velocity extraction.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(hypercompression_of_time_horizons, extractiveness, 0.88).
narrative_ontology:constraint_metric(hypercompression_of_time_horizons, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(hypercompression_of_time_horizons, theater_ratio, 0.85).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The steward is trapped: holding for the long-term results in immediate 
% capital liquidation by high-frequency competitors, forcing them into the "Now."
constraint_indexing:constraint_classification(hypercompression_of_time_horizons, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The algorithm views the compression as a Rope—the essential coordination 
% substrate for achieving maximum liquidity and price discovery in real-time.
constraint_indexing:constraint_classification(hypercompression_of_time_horizons, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: the "Quarterly Vision" 
% is an inertial spike; it performatively signals the future while siphoning it.
constraint_indexing:constraint_classification(hypercompression_of_time_horizons, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) and coordination necessity as a hybrid.
constraint_indexing:constraint_classification(hypercompression_of_time_horizons, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(hypercompression_of_time_horizons, E), E >= 0.50,
    domain_priors:suppression_score(hypercompression_of_time_horizons, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hypercompression_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless steward vs Rope for the institutional algorithm.
    constraint_indexing:constraint_classification(hypercompression_of_time_horizons, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hypercompression_of_time_horizons, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.85) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(hypercompression_of_time_horizons, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(hypercompression_of_time_horizons, E),

    E > 0.70.

:- end_tests(hypercompression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of real-time efficiency is achieved by 
 * liquidating the species' ability to maintain a long-term horizon.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Long-Term Steward feels a Snare because they are forced to participate 
 * in a "burning of the future" to survive the fiscal quarter. The Algorithm 
 * sees a Rope because the compression coordinates a perfectly responsive 
 * global economy.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Efficient Market" is no longer functional for species 
 * survival (Theater 0.85); it is an inert spike siphoning 0.88 of 
 * the future territory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_temporal_exhaustion,
    'When the "future" is fully liquidated, does the system collapse or find a new Rope (Snare vs Mountain)?',
    'Tracking the failure rate of long-term infrastructure projects in high-velocity economies.',
    'If projects fail: Snare of current incentives. If they hold: Mountain of Social Resilience.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hypercompression_of_time_horizons, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
