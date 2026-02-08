% ============================================================================
% CONSTRAINT STORY: shock_propagation_asymmetry
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_shock_propagation_asymmetry, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: shock_propagation_asymmetry
 * human_readable: The One-Way Crisis Valve
 * domain: economic/logistical
 * * SUMMARY:
 * A global market integration framework that ensures positive gains (growth, efficiency) are concentrated at the center, while negative shocks (inflation, supply failures, environmental debt) are funneled exclusively to the periphery. This coordination substrate becomes a "Snare" for the peripheral subject, as their economic and physical agency is liquidated to act as a "shock absorber" for the central institution, trapping them in a territory of permanent volatility with no share in the corresponding stability.
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
domain_priors:suppression_score(shock_propagation_asymmetry, 0.82).   % Exit options or defensive hedges are suppressed by center-aligned trade mandates.
domain_priors:theater_ratio(shock_propagation_asymmetry, 0.89).       % High theater: "Emergency Relief Funds" that performatively signal care while 0.90 extraction occurs.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(shock_propagation_asymmetry, extractiveness, 0.90).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(shock_propagation_asymmetry, theater_ratio, 0.89).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a necessary coordination mechanism for global stability.
narrative_ontology:constraint_claim(shock_propagation_asymmetry, coordination).

% Binary flags
% This system requires active enforcement through trade agreements and financial policy.
domain_priors:requires_active_enforcement(shock_propagation_asymmetry).

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, central_clearinghouse).
narrative_ontology:constraint_victim(shock_propagation_asymmetry, peripheral_producer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
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

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: a genuine coordination function (stabilizing the core)
% that is inextricably linked with severe, asymmetric extraction.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% With a theater ratio of 0.89, the performative aspects (e.g., "relief funds")
% are so dominant that the system can be seen as an inert, theatrical mechanism
% that fails to perform its stated function of equitable stability.
constraint_indexing:constraint_classification(shock_propagation_asymmetry, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shock_propagation_asymmetry_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless producer vs Rope for the institutional clearinghouse.
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.89) correctly triggers the Piton classification.
    domain_priors:theater_ratio(shock_propagation_asymmetry, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(shock_propagation_asymmetry, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that all structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(shock_propagation_asymmetry, _),
    narrative_ontology:constraint_victim(shock_propagation_asymmetry, _),
    domain_priors:requires_active_enforcement(shock_propagation_asymmetry).

:- end_tests(shock_propagation_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extreme extraction score (0.90) and suppression (0.82) model a system
 * where the stability of the core is directly purchased by liquidating the
 * agency and stability of the periphery. The high theater ratio (0.89) reflects
 * the performative "aid" and "development" programs that mask this extractive
 * transfer, making the system appear functional or even benevolent.
 *
 * The perspectival gap is severe: the beneficiary (Central Clearinghouse)
 * experiences a pure Rope that coordinates global markets for its benefit. The
 * victim (Peripheral Producer) experiences a pure Snare, where their participation
 * is mandatory but results in the siphoning of their economic resilience.
 *
 * * MANDATROPHY ANALYSIS:
 * This constraint is a classic case of Mandatrophy, where a system with a
 * legitimate coordination function becomes pathologically extractive. A simple
 * Snare classification would be inaccurate because it would miss the coordination
 * element that makes the system so stable and difficult to replace. The
 * Tangled Rope classification resolves this by acknowledging both functions
 * simultaneously: it is a coordination mechanism (Rope) that has been weaponized
 * for asymmetric extraction (Snare). This prevents policy errors that might
 * try to dismantle the "Snare" without providing an alternative for the
 * "Rope" function, leading to systemic collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_shock_propagation_asymmetry,
    'Is the observed asymmetry an artifact of the current network topology (a constructed Snare) or an unavoidable physical law of hub-and-spoke networks (a Mountain)?',
    'Comparative analysis of shock propagation in decentralized vs. centralized logistical networks during major global crises (e.g., 2026-style events).',
    'If decentralized networks show symmetric shock absorption, the current system is a Snare. If they also centralize risk, it points to a Mountain of network physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(shock_propagation_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This high-extraction constraint evolved over time. Initially a less extractive
% system, it intensified as core actors optimized it for their benefit.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(spa_tr_t0, shock_propagation_asymmetry, theater_ratio, 0, 0.60).
narrative_ontology:measurement(spa_tr_t5, shock_propagation_asymmetry, theater_ratio, 5, 0.75).
narrative_ontology:measurement(spa_tr_t10, shock_propagation_asymmetry, theater_ratio, 10, 0.89).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(spa_ex_t0, shock_propagation_asymmetry, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(spa_ex_t5, shock_propagation_asymmetry, base_extractiveness, 5, 0.85).
narrative_ontology:measurement(spa_ex_t10, shock_propagation_asymmetry, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The system functions as a form of global infrastructure for market stability.
narrative_ontology:coordination_type(shock_propagation_asymmetry, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */