% ============================================================================
% CONSTRAINT STORY: shock_propagation_asymmetry
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(shock_propagation_asymmetry, []).

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
 * * constraint_id: shock_propagation_asymmetry
 * human_readable: The One-Way Crisis Valve
 * domain: economic/logistical/sociological
 * * SUMMARY:
 * A scenario where a "Rope" for global market or logistical integration ensures 
 * that positive gains (growth, efficiency) are concentrated at the center, 
 * while negative shocks (inflation, supply failures, environmental debt) are 
 * funneled exclusively to the periphery. This coordination substrate becomes 
 * a "Snare" for the peripheral subject, as their economic and physical agency 
 * is liquidated to act as a "shock absorber" for the central institution, 
 * trapping them in a territory of permanent volatility with no share in 
 * the corresponding stability.
 * * KEY AGENTS:
 * - Peripheral Producer: Subject (Powerless)
 * - Central Clearinghouse: Beneficiary (Institutional)
 * - Macro-Stability Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.90) reflects the parasitic liquidation of the subject's 
% stability to maintain the center's "frictionless" coordination Rope.
domain_priors:base_extractiveness(shock_propagation_asymmetry, 0.90). 
domain_priors:suppression_score(shock_propagation_asymmetry, 0.82). % Exit options or defensive hedges are suppressed by center-aligned trade mandates.
domain_priors:theater_ratio(shock_propagation_asymmetry, 0.89).    % High theater: "Emergency Relief Funds" that performatively signal care while 0.90 extraction occurs.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(shock_propagation_asymmetry, extractiveness, 0.9).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, theater_ratio, 0.89).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The producer is trapped: they must use the global Rope to sell their output, 
% but the asymmetric valves liquidate their primary economic agency.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The clearinghouse views the asymmetry as a Rope—the essential coordination 
% substrate for ensuring "Systemic Stability" at the global core.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.89) > 0.70 triggers Piton: the "Stabilization Protocol" 
% is an inertial spike; it performatively catalogs the shock while permitting extraction.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.90) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(shock_propagation_asymmetry, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(shock_propagation_asymmetry, E), E >= 0.50,
    domain_priors:suppression_score(shock_propagation_asymmetry, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shock_propagation_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless producer vs Rope for the institutional clearinghouse.
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare, 
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, rope, 
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.89) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, piton, 
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % High extraction (0.90) > 0.70 requires [RESOLVED MANDATROPHY] hook.
    domain_priors:base_extractiveness(shock_propagation_asymmetry, E),

    E > 0.70.

:- end_tests(shock_propagation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.90) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a stable core is achieved by liquidating the 
 * peripheral subject's primary survival agency.
 *
 * 
 *
 * * PERSPECTIVAL GAP:
 * The Peripheral Producer feels a Snare because they pay the full price 
 * of systemic failures they did not initiate. The Central Clearinghouse 
 * sees a Rope because the asymmetric propagation coordinates the global 
 * price stability required for mass-market growth.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Fair Trade Certification" is no longer functional (Theater 0.89); 
 * it is an inert spike siphoning 0.90 of the producer's agency. 
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_shock_elasticity,
    'Can "Decentralized Liquidity" restore the Rope, or is asymmetry a physical law of network hubs (Snare vs Mountain)?',
    'Tracking the recovery rate of peer-to-peer trade networks versus hub-centric networks during 2026-style crises.',
    'If P2P networks stabilize agency: Snare of current design. If they collapse: Mountain of Network Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(shock_propagation_asymmetry, 0, 10). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
