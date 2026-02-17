% ============================================================================
% CONSTRAINT STORY: nsw_transmission_bottleneck
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-13
% ============================================================================

:- module(constraint_nsw_transmission_bottleneck, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * constraint_id: nsw_transmission_bottleneck
 * human_readable: NSW Regional Transmission Congestion
 * domain: technological/political
 *
 * SUMMARY:
 * The physical limit on megawatts that can be transferred from South Australia
 * to New South Wales. While SA experiences negative prices and surplus solar
 * generation, the NSW grid remains reliant on expensive local gas
 * and coal generation because the transmission pipes are "full." This acts
 * as a Snare for NSW consumers and a barrier to national decarbonization.
 *
 * KEY AGENTS (by structural relationship):
 * - nsw_heavy_industry: Primary target (powerless/trapped) — cannot exit the
 * local price zone due to physical grid reality.
 * - gas_peaker_plants_nsw: Primary beneficiary (institutional/arbitrage) —
 * profit from high local spot prices caused by the lack of SA renewable inflow.
 * - transgrid_operator: Analytical observer — manages the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

domain_priors:base_extractiveness(nsw_transmission_bottleneck, 0.58).
domain_priors:suppression_score(nsw_transmission_bottleneck, 0.72). % High physical foreclosure of alternatives.
domain_priors:theater_ratio(nsw_transmission_bottleneck, 0.20).    % Functional but congested.

narrative_ontology:constraint_metric(nsw_transmission_bottleneck, extractiveness, 0.58).
narrative_ontology:constraint_metric(nsw_transmission_bottleneck, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(nsw_transmission_bottleneck, theater_ratio, 0.20).

narrative_ontology:constraint_claim(nsw_transmission_bottleneck, snare).
narrative_ontology:human_readable(nsw_transmission_bottleneck, "NSW Regional Transmission Congestion").

% Structural relationships
narrative_ontology:constraint_beneficiary(nsw_transmission_bottleneck, gas_peaker_plants_nsw).
narrative_ontology:constraint_victim(nsw_transmission_bottleneck, nsw_heavy_industry).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (NSW INDUSTRY)
% Perception: Snare. Locked into higher energy costs with no immediate exit.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (LOCAL GAS GENERATORS)
% Perception: Rope. Maintains market viability and regional grid "independence."
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SYSTEM AUDITOR)
% Perception: Snare. High extraction (price delta) due to physical under-capacity.
constraint_indexing:constraint_classification(nsw_transmission_bottleneck, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nsw_transmission_bottleneck_tests).

test(extraction_threshold) :-
    domain_priors:base_extractiveness(nsw_transmission_bottleneck, E),
    E >= 0.46. % Correctly identifies as a high-extraction Snare.

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nsw_transmission_bottleneck, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nsw_transmission_bottleneck, rope, context(agent_power(institutional), _, _, _)).

:- end_tests(nsw_transmission_bottleneck_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The bottleneck is a Snare because it forces a transfer of wealth from NSW
 * consumers to high-marginal-cost generators, whereas the "alternatives"
 * (SA renewables) are physically present but structurally inaccessible.
 * The base extractiveness of 0.58 reflects the significant price differential
 * between the two regions that is captured by incumbent generators. Suppression
 * is high (0.72) because the alternative is physically foreclosed by grid capacity.
 *
 * DIRECTIONALITY LOGIC:
 * NSW Industry is the victim because their energy intensity makes them
 * "trapped" in the regional pricing node. Gas peakers in NSW benefit
 * because the lack of competition from SA wind/solar allows them to set
 * the marginal price at higher levels. This is a classic structural wealth
 * transfer enabled by a physical chokepoint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(
    omega_pec_completion,
    'Will Project EnergyConnect eliminate the bottleneck or simply shift the congestion further upstream?',
    'Review of interconnector flow data in late 2026/2027.',
    'Resolution = Piton (Residual Bottleneck) vs. Scaffold (Solved Bottleneck)',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nsw_transmission_bottleneck, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction rises as the SA/NSW price delta widens due to increasing
% renewable penetration in SA, making the bottleneck more profitable for incumbents.
narrative_ontology:measurement(bottleneck_ex_t0, nsw_transmission_bottleneck, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(bottleneck_ex_t5, nsw_transmission_bottleneck, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(bottleneck_ex_t10, nsw_transmission_bottleneck, base_extractiveness, 10, 0.58).

% Theater ratio is low and stable, as the constraint is a physical reality,
% not a performative one. It is functional, just insufficient.
narrative_ontology:measurement(bottleneck_tr_t0, nsw_transmission_bottleneck, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bottleneck_tr_t5, nsw_transmission_bottleneck, theater_ratio, 5, 0.22).
narrative_ontology:measurement(bottleneck_tr_t10, nsw_transmission_bottleneck, theater_ratio, 10, 0.20).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nsw_transmission_bottleneck, global_infrastructure).

% Network Relationship: The bottleneck directly drives the price differential story.
narrative_ontology:affects_constraint(nsw_transmission_bottleneck, sa_renewable_price_differential).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */