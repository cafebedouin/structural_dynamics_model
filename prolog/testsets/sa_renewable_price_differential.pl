% ============================================================================
% CONSTRAINT STORY: sa_renewable_price_differential
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-13
% ============================================================================

:- module(constraint_sa_renewable_price_differential, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
    constraint_indexing:directionality_override/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: sa_renewable_price_differential
 * human_readable: SA Renewable Price Arbitrage Proxy
 * domain: economic/technological
 *
 * SUMMARY:
 * As of Feb 2026, South Australia (SA) has achieved 84%+ renewable penetration,
 * driving wholesale prices to $37/MWh (the lowest in the NEM) while NSW remains 
 * at $75/MWh. This differential acts as a "real-time cost proxy" for the
 * absence of battery-supported capacity.
 *
 * KEY AGENTS (by structural relationship):
 * - nsw_industrial_consumers: Primary target (powerless/trapped) — bear the 
 * extraction of the "firming gap" via higher spot prices.
 * - sa_bess_operators: Primary beneficiary (institutional/arbitrage) — 
 * capture value during 48.4% negative price intervals.
 * - aemo_regulator: Analytical observer — manages inter-regional coordination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

domain_priors:base_extractiveness(sa_renewable_price_differential, 0.42).
domain_priors:suppression_score(sa_renewable_price_differential, 0.55). % Lack of interconnector capacity.
domain_priors:theater_ratio(sa_renewable_price_differential, 0.15).    % High functional efficiency.

narrative_ontology:constraint_metric(sa_renewable_price_differential, extractiveness, 0.42).
narrative_ontology:constraint_metric(sa_renewable_price_differential, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(sa_renewable_price_differential, theater_ratio, 0.15).

narrative_ontology:constraint_claim(sa_renewable_price_differential, tangled_rope).
narrative_ontology:human_readable(sa_renewable_price_differential, "SA Renewable Price Arbitrage Proxy").

domain_priors:requires_active_enforcement(sa_renewable_price_differential). % Market bidding rules.

% Structural relationships
narrative_ontology:constraint_beneficiary(sa_renewable_price_differential, sa_bess_operators).
narrative_ontology:constraint_victim(sa_renewable_price_differential, nsw_industrial_consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (NSW CONSUMER)
% Perception: Snare. High prices are unavoidable until local firming arrives.
constraint_indexing:constraint_classification(sa_renewable_price_differential, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (SA BESS OPERATOR)
% Perception: Rope. A highly efficient tool for grid coordination and profit.
constraint_indexing:constraint_classification(sa_renewable_price_differential, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (AEMO)
% Perception: Tangled Rope. Essential coordination with asymmetric regional extraction.
constraint_indexing:constraint_classification(sa_renewable_price_differential, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sa_renewable_price_differential_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sa_renewable_price_differential, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sa_renewable_price_differential, rope, context(agent_power(institutional), _, _, _)).

test(extraction_logic) :-
    domain_priors:base_extractiveness(sa_renewable_price_differential, E),
    E > 0.30, E < 0.50.

:- end_tests(sa_renewable_price_differential_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The differential (SA: $37 vs NSW: $75) isn't just a price gap; it's a 
 * structural constraint on economic activity. The "extraction" is the 
 * opportunity cost of not having transitioned to the SA model (renewables 
 * + batteries). NSW consumers effectively pay a "fossil fuel tax" relative 
 * to the SA benchmark.
 *
 * DIRECTIONALITY LOGIC:
 * SA Beneficiaries (BESS/VPP operators) enjoy negative prices (~48% of the time), 
 * charging for free (or being paid to charge) and discharging during peak. 
 * NSW Victims (Industrial) are trapped by the physical limits of the existing 
 * inter-regional transmission lines (Project EnergyConnect still testing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    omega_sa_diff,
    'Will the price differential collapse once Project EnergyConnect is fully operational in late 2026?',
    'AEMO Quarterly Energy Dynamics Q4 2026 reporting inter-regional spot price convergence.',
    'Convergence = Rope (Pure Coordination); Sustained Divergence = Snare (Infrastructure Bottleneck)',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Show the transition from high-extraction (volatile gas) to lower extraction (VRE).
narrative_ontology:measurement(sa_ex_t0, sa_renewable_price_differential, base_extractiveness, 0, 0.65). % 2024: High volatility
narrative_ontology:measurement(sa_ex_t5, sa_renewable_price_differential, base_extractiveness, 5, 0.48). % 2025: Transition
narrative_ontology:measurement(sa_ex_t10, sa_renewable_price_differential, base_extractiveness, 10, 0.42). % 2026: Low wholesale avg.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sa_renewable_price_differential, resource_allocation).
narrative_ontology:affects_constraint(sa_renewable_price_differential, nsw_industrial_competitiveness).

% Required for external script parsing
narrative_ontology:interval(sa_renewable_price_differential, 0, 10).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
