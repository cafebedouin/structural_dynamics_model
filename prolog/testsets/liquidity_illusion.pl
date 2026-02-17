% ============================================================================
% CONSTRAINT STORY: liquidity_illusion
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_liquidity_illusion, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: liquidity_illusion
 * human_readable: The Exit Door Mirage
 * domain: economic/technological
 * * SUMMARY:
 * A scenario where a financial or digital market maintains the appearance of
 * high liquidity (easy entry/exit) during normal conditions. This "Rope" of
 * coordination encourages mass participation, but the liquidity is a
 * "Piton" or "Snare" because it vanishes the moment a systemic shock occurs,
 * leaving all participants trapped in a crowded room with a shrinking exit.
 * * KEY AGENTS:
 * - Retail Trader: Subject (Powerless)
 * - Market Maker / Exchange: Beneficiary (Institutional)
 * - Financial Stability Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) as the illusion allows the institution to capture
% the "liquidity premium" while externalizing the risk of a "flash crash"
% or bank run onto the subject.
domain_priors:base_extractiveness(liquidity_illusion, 0.85).
domain_priors:suppression_score(liquidity_illusion, 0.68).
domain_priors:theater_ratio(liquidity_illusion, 0.92). % Extreme theater: High-frequency bids that disappear on stress.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(liquidity_illusion, extractiveness, 0.85).
narrative_ontology:constraint_metric(liquidity_illusion, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(liquidity_illusion, theater_ratio, 0.92).

% The constraint claims to be a coordination mechanism for efficient markets.
narrative_ontology:constraint_claim(liquidity_illusion, tangled_rope).
narrative_ontology:human_readable(liquidity_illusion, "The Exit Door Mirage").

% Binary flags and structural properties for Tangled Rope classification.
domain_priors:requires_active_enforcement(liquidity_illusion). % Enforcement via automated market rules and permissive regulation.

% Structural property derivation hooks for Tangled Rope.
narrative_ontology:constraint_beneficiary(liquidity_illusion, market_makers_and_exchanges).
narrative_ontology:constraint_victim(liquidity_illusion, retail_traders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped: the "instant exit" they were promised is
% unavailable precisely when it is needed most.
constraint_indexing:constraint_classification(liquidity_illusion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views the deep order book (even if illusory) as a Rope—
% the only way to coordinate mass investment and capital efficiency.
constraint_indexing:constraint_classification(liquidity_illusion, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid signature of coordination intent (Rope)
% entangled with predatory risk-concealment (Snare). This is the canonical
% classification based on the structural properties.
constraint_indexing:constraint_classification(liquidity_illusion, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.92) > 0.70 triggers Piton: the "liquidity" is a
% non-functional, performative artifact that does not withstand pressure.
constraint_indexing:constraint_classification(liquidity_illusion, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liquidity_illusion_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institutional beneficiary.
    constraint_indexing:constraint_classification(liquidity_illusion, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liquidity_illusion, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(liquidity_illusion, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.92) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(liquidity_illusion, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensure extraction (0.85) is in the high-extraction range for Snare/Tangled Rope.
    narrative_ontology:constraint_metric(liquidity_illusion, extractiveness, E),
    E >= 0.46.

test(tangled_rope_properties_present) :-
    % Verify all three structural requirements for Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(liquidity_illusion, _),
    narrative_ontology:constraint_victim(liquidity_illusion, _),
    domain_priors:requires_active_enforcement(liquidity_illusion).

:- end_tests(liquidity_illusion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models the fragility of modern, algorithmically-driven markets.
 * The base extractiveness (0.85) is high because market makers and exchanges
 * profit from the volume attracted by the illusion of liquidity, while the systemic
 * risk of a liquidity crisis is borne entirely by participants. The theater ratio
 * (0.92) is exceptionally high, representing high-frequency trading orders that
 * are placed and canceled within milliseconds, creating a facade of market depth
 * that is not functionally available during a stress event.
 *
 * * PERSPECTIVAL GAP:
 * The Retail Trader experiences a Snare because they are lured into a market by
 * the promise of easy exit, a promise that is revoked when most needed. The Market
 * Maker sees a Rope because this illusion is the very mechanism that coordinates
 * capital and generates volume, which is their business model.
 *
 * * [RESOLVED MANDATROPHY]:
 * The system avoids misclassifying this as a pure Snare by recognizing its
 * dual nature as a Tangled Rope. The presence of beneficiaries (market makers)
 * and a genuine (if fragile) coordination function, alongside victims (traders)
 * and active enforcement (market rules), confirms the hybrid classification. The
 * Piton classification further refines this by highlighting the functional
 * decay of the core "liquidity" promise, which has become almost entirely theatrical.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_liquidity_illusion,
    'Is the fragility of algorithmic liquidity an intentional feature for extraction (Snare) or an emergent, unavoidable property of complex systems (Mountain)?',
    'Analysis of exchange rule-making processes and HFT firm profit models during volatility events.',
    'If intentional: Snare/Tangled Rope of regulatory capture. If emergent: Mountain of computational complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(liquidity_illusion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified as markets became more automated and high-frequency
% trading dominated. The initial state had some extraction and theater, but it
% grew significantly over the interval.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(liquidity_illusion_tr_t0, liquidity_illusion, theater_ratio, 0, 0.40).
narrative_ontology:measurement(liquidity_illusion_tr_t5, liquidity_illusion, theater_ratio, 5, 0.75).
narrative_ontology:measurement(liquidity_illusion_tr_t10, liquidity_illusion, theater_ratio, 10, 0.92).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(liquidity_illusion_ex_t0, liquidity_illusion, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(liquidity_illusion_ex_t5, liquidity_illusion, base_extractiveness, 5, 0.70).
narrative_ontology:measurement(liquidity_illusion_ex_t10, liquidity_illusion, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% The constraint's coordination function is the allocation of capital.
narrative_ontology:coordination_type(liquidity_illusion, resource_allocation).

% The stability of this constraint directly impacts overall market stability.
narrative_ontology:affects_constraint(liquidity_illusion, market_stability).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */