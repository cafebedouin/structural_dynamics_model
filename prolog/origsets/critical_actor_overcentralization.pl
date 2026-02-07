% ============================================================================
% CONSTRAINT STORY: critical_actor_overcentralization
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(critical_actor_overcentralization, []).

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
 * * constraint_id: critical_actor_overcentralization
 * human_readable: The Single Point of Failure
 * domain: logistical/technological/economic
 * * SUMMARY:
 * A scenario where a network’s functional survival depends entirely on a single 
 * node—be it a clearinghouse bank, a cloud provider, or a charismatic leader. 
 * This "Rope" for achieving massive coordination efficiency and 
 * standardization becomes a "Snare" for the peripheral nodes, whose survival 
 * agency is liquidated because they have no exit options or redundant 
 * pathways. When the center falters or chooses to extract higher rents, 
 * the periphery is trapped in a terminal dependency.
 *
 * * KEY AGENTS:
 * - Peripheral Node (Small Business): Subject (Powerless)
 * - Central Clearinghouse: Beneficiary (Institutional)
 * - Systemic Risk Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the near-total siphoning of the periphery's 
% strategic agency to maintain the center's dominance.
domain_priors:base_extractiveness(critical_actor_overcentralization, 0.88). 
domain_priors:suppression_score(critical_actor_overcentralization, 0.78). % Decentralized alternatives are suppressed by "efficiency" mandates and high barriers to entry.
domain_priors:theater_ratio(critical_actor_overcentralization, 0.93).    % Extreme theater: "Redundancy Drills" and "Service Level Agreements" that fail to account for true systemic collapse.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(critical_actor_overcentralization, extractiveness, 0.88).
narrative_ontology:constraint_metric(critical_actor_overcentralization, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(critical_actor_overcentralization, theater_ratio, 0.93).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The peripheral node is trapped: they cannot survive without the hub, 
% yet the hub's existence liquidates their ability to ever become autonomous.
constraint_indexing:constraint_classification(critical_actor_overcentralization, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The center views the overcentralization as a Rope—the only way to coordinate 
% global-scale stability and ensure a legible, frictionless economy.
constraint_indexing:constraint_classification(critical_actor_overcentralization, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.93) > 0.70 triggers Piton: the "Risk Management Committee" 
% is an inertial spike; it performatively catalogs risks while permitting 0.88 extraction.
constraint_indexing:constraint_classification(critical_actor_overcentralization, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.88) and high theater (0.93) as a toxic hybrid signature.
constraint_indexing:constraint_classification(critical_actor_overcentralization, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(critical_actor_overcentralization, E), E >= 0.50,
    domain_priors:suppression_score(critical_actor_overcentralization, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(critical_actor_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless periphery vs Rope for the institutional center.
    constraint_indexing:constraint_classification(critical_actor_overcentralization, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(critical_actor_overcentralization, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.93) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(critical_actor_overcentralization, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (> 0.70) requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(critical_actor_overcentralization, E),

    E > 0.70.

:- end_tests(critical_actor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a central authority is achieved by liquidating 
 * the resilience and agency of all sub-nodes in the network.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Peripheral Node feels a Snare because their "efficiency" is actually 
 * a fragility they cannot escape. The Central Hub sees a Rope because 
 * centralization coordinates the massive volume of traffic necessary 
 * to stay globally competitive.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Failover Protocol" is no longer functional (Theater 0.93); 
 * it is an inert spike siphoning 0.88 of the network's collective agency. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_hub_collapse_velocity,
    'Does a hub collapse into a "Mountain" of chaos, or can decentralized Ropes be deployed in time (Snare vs Mountain)?',
    'Tracking the time-to-recovery of decentralized networks versus centralized hubs during 2026-style outages.',
    'If decentralized nodes recover faster: Snare of current hub policy. If they fail to coordinate: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(critical_actor_overcentralization, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
