% ============================================================================
% CONSTRAINT STORY: demand_response_cost_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_demand_response_cost_shift, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: demand_response_cost_shift
 *   human_readable: Smart Grid Demand Response Rate Structure
 *   domain: economic/technological
 *
 * SUMMARY:
 *   Demand Response (DR) programs use time-of-use pricing and dynamic rate
 *   structures to incentivize consumers to shift electricity consumption away
 *   from peak hours, thereby stabilizing grid frequency and reducing peak
 *   generation capacity requirements. The constraint exhibits a structural
 *   tension between two legitimate functions: (1) coordination of demand-side
 *   flexibility for grid stability, and (2) cost recovery that is distributed
 *   asymmetrically across income levels. Households with flexible schedules,
 *   controllable loads (pool pumps, EV chargers, water heaters), and
 *   sufficient wealth to invest in smart thermostats can benefit from
 *   off-peak discounts. Households with inflexible consumption patterns
 *   (shift workers, families with young children, those in climates with
 *   extreme temperatures) face higher bills during peak hours with minimal
 *   ability to respond. The extractiveness value (0.52) reflects that the
 *   program transfers costs from flexible to inflexible consumers while
 *   providing grid benefits that theoretically accrue to all. The suppression
 *   value (0.58) reflects the multiple barriers to exit: grid dependency is
 *   non-negotiable, alternative billing options are absent, and regulatory
 *   protections for vulnerable populations are minimal. Theater ratio (0.64)
 *   reflects that a portion of demand response effectiveness comes from
 *   behavioral response to prices rather than from actual consumption
 *   reduction — some consumers may simply shift consumption to adjacent hours
 *   rather than reduce it, or may face psychic costs from living with
 *   temperature setpoints outside comfort ranges. The constraint's
 *   classification as Tangled Rope emerges from the combination of genuine
 *   coordination function (grid stability) and asymmetric extraction (cost
 *   burden on inflexible populations), with active enforcement required to
 *   prevent degeneration into pure extraction.
 *
 * KEY AGENTS:
 *   - Inflexible Low-Income Household: Primary victim (powerless/trapped) — cannot shift consumption, faces higher peak-hour rates with minimal ability to respond or exit the grid
 *   - Partially Flexible Consumer: Secondary victim (moderate/constrained) — can shift some loads (water heating, EV charging) but faces behavioral costs and capital requirements
 *   - Grid Operator: Primary beneficiary (institutional/arbitrage) — achieves frequency stability and reduced peak capacity needs; can arbitrage between peak and off-peak operations
 *   - Peak-Hour Producer: Secondary beneficiary (powerful/arbitrage) — faces reduced peak demand, lower marginal generation costs, and arbitrage opportunities
 *   - Consumer Advocacy Coalition: Organized agent (organized/mobile) — sees both coordination benefit and equity cost; advocates for rate protections and alternative mechanisms
 *   - Regulatory Authority: Institutional actor (organized/mobile) — implements demand response as temporary mechanism with long-term transition to supply-side alternatives (storage, distributed generation)
 *   - Legacy Rate-Setting Bureaucracy: Institutional actor (institutional/constrained) — maintains demand response programs through inertia; original function increasingly performative as grid decentralizes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demand_response_cost_shift, 0.52).
domain_priors:suppression_score(demand_response_cost_shift, 0.58).
domain_priors:theater_ratio(demand_response_cost_shift, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demand_response_cost_shift, extractiveness, 0.52).
narrative_ontology:constraint_metric(demand_response_cost_shift, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(demand_response_cost_shift, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demand_response_cost_shift, tangled_rope).
narrative_ontology:human_readable(demand_response_cost_shift, "Smart Grid Demand Response Rate Structure").
narrative_ontology:topic_domain(demand_response_cost_shift, "economic/technological").

domain_priors:requires_active_enforcement(demand_response_cost_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, peak_hour_producers).
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, grid_operators).
narrative_ontology:constraint_victim(demand_response_cost_shift, inflexible_consumers).
narrative_ontology:constraint_victim(demand_response_cost_shift, low_income_households).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFLEXIBLE LOW-INCOME HOUSEHOLD (SNARE) — Cannot shift consumption (working hours, heating/cooling needs are fixed); trapped on the grid with no alternative; bears full cost of peak-hour rates while benefiting minimally from off-peak discounts. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(demand_response_cost_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PARTIALLY FLEXIBLE CONSUMER (TANGLED ROPE) — Can shift some consumption (water heating, pool pumps, EV charging) but faces behavioral costs and appliance constraints; benefits from off-peak discounts but also bears some peak-hour costs; active participation required. d≈0.58, f(d)≈0.68, σ=0.9 → χ≈0.32.
constraint_indexing:constraint_classification(demand_response_cost_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GRID OPERATOR (ROPE) — Benefits from demand reduction during peak hours; experiences constraint as coordination mechanism for frequency stability; can arbitrage between peak and off-peak operations; has exit option (alternative grid technologies like storage). d≈0.08, f(d)≈-0.09, σ=1.1 → χ≈-0.05.
constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PEAK-HOUR PRODUCER (ROPE) — Benefits from reduced peak demand (lower marginal generation costs); can arbitrage by operating in peak markets; experiences constraint as coordination of production scheduling. d≈0.10, f(d)≈-0.07, σ=1.1 → χ≈-0.03.
constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER ADVOCACY COALITION (TANGLED ROPE) — Organized agents (consumer unions, environmental groups) see both coordination benefit (grid stability, renewable integration) and extraction (cost shifting to vulnerable populations); mobile exit (can shift advocacy to alternative grid models like community solar); active enforcement required to maintain fairness protections. d≈0.45, f(d)≈0.50, σ=1.0 → χ≈0.26.
constraint_indexing:constraint_classification(demand_response_cost_shift, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATORY AUTHORITY (SCAFFOLD) — Implements demand response as temporary coordination mechanism with sunset: as energy storage capacity grows and distributed generation scales, the need for consumption-side flexibility declines. Sees pathway to alternative balancing mechanisms (battery storage, demand-side management) that reduce reliance on peak-hour rate discrimination. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.12.
constraint_indexing:constraint_classification(demand_response_cost_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: LEGACY RATE-SETTING BUREAUCRACY (PITON) — Maintains demand response programs through institutional inertia; original coordination function (balancing peak demand) is increasingly performative as distributed storage and renewable production decentralize grid operations; theater_ratio=0.64 reflects that much of the peak-hour rate differentiation persists more for cost recovery than grid necessity. d≈0.20, f(d)≈0.08, σ=0.9 → χ≈0.06.
constraint_indexing:constraint_classification(demand_response_cost_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICS VIEW (MOUNTAIN) — From a physical standpoint, grid frequency stability requires real-time balancing of supply and demand; peak demand must either be met by generation or reduced by consumption — this is an immutable physical constraint. However, the structural data (ε=0.52, suppression=0.58, theater=0.64) reveals that institutional arrangements (rate structures, contract design) are contingent, not natural. The false summit detector flags this perspective as misapplying natural law framing to a tangled_rope constraint.
constraint_indexing:constraint_classification(demand_response_cost_shift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(demand_response_cost_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(demand_response_cost_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(demand_response_cost_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(demand_response_cost_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(demand_response_cost_shift, TR),
    TR >= 0.70.

:- end_tests(demand_response_cost_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The program transfers peak-hour costs from flexible to inflexible consumers while providing grid benefits. The extraction is not as severe as pure monopoly pricing (0.70+) because grid stability is a genuine coordination need and off-peak discounts do benefit some participants. However, the extraction is significant because inflexible populations (low-income, service workers, families with young children) bear costs while flexible populations (wealthy, time-flexible, large-load owners) capture benefits. The measurement trajectory (0.28→0.52 over the interval) reflects accumulating cost shifts as smart meters expand and rate differentiation deepens. Suppression (0.58): Moderate-high. Multiple barriers prevent exit or response: grid dependency is absolute, alternative providers are unavailable in most jurisdictions, capital requirements for smart appliances are high, and behavioral adaptation has limits (room temperature cannot go below safety thresholds, elderly and young children have temperature sensitivity). However, suppression is not total (0.60+) because some consumption flexibility exists and regulatory protections have been implemented in some jurisdictions. Theater ratio (0.64): Moderate-high. A portion of demand response effectiveness comes from genuine consumption reduction (coordination function), but a significant portion reflects behavioral responses that may shift rather than reduce consumption, or impose psychic/health costs. The rise over the interval (0.42→0.64) reflects increasing sophistication in rate design, which makes the coordination function more performative — moving to second-order price signals designed to trigger behavioral response rather than to meet structural grid needs.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp divergence between beneficiaries and victims. The grid operator and peak-hour producer see Rope (genuine coordination with arbitrage options), while the inflexible household sees Snare (extraction with no escape). The regulatory authority sees Scaffold (temporary mechanism with technology-driven sunset as storage scales), while the legacy bureaucracy sees Piton (performative rate-setting maintaining historical structure). The consumer coalition sees Tangled Rope (genuine coordination paired with inequitable extraction that can be reformed), while the analytical observer risks seeing Mountain (naturalizing grid physics constraints as requiring this specific rate design). The perspectival gaps arise from asymmetric exit options: flexible consumers can arbitrage between peak and off-peak rates; inflexible consumers cannot. The regulatory authority has agency to phase toward alternatives; the legacy bureaucracy is constrained by path dependency. This is not a measurement ambiguity but a genuine structural difference in how the constraint operates on different agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Inflexible household: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximum extraction. No exit options, high cost exposure, minimal benefits from coordination. Partially flexible consumer: Victim + constrained → d≈0.58, f(d)≈0.68. Moderate extraction. Some capacity to shift consumption, some benefit from off-peak discounts, but also constrained by appliance technology and behavioral limits. Grid operator: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. Can arbitrage across time periods and has alternative technologies (storage). Peak-hour producer: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.07. Net beneficiary with exit options. Consumer coalition: Victim-advocate + mobile → d≈0.45, f(d)≈0.50. Mixed but with agency. Organized and can exit toward alternative advocacy positions (community solar, distributed generation). Regulatory authority: Hybrid + mobile → d≈0.35, f(d)≈0.35. Can shift toward alternative mechanisms; sunset logic applies. Legacy bureaucracy: Beneficiary-defender + constrained → d≈0.20, f(d)≈0.08. Path-dependent institution maintaining structure; low effective extraction because piton classification (theater gate, not chi gate) applies.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL AMBIGUITY: The mandate-atrophy tension here is whether demand response can be implemented as genuine Rope (pure coordination) or whether the rate structure inherently becomes Snare (extraction). The structural resolution: Demand response CAN be designed as Rope through universal consumption flexibility (community solar, demand-side management that scales with income level, storage access distributed equitably). However, THE SPECIFIC IMPLEMENTATION via time-of-use rate differentiation on the legacy billing structure is Tangled Rope or Snare because it leverages existing inequality (flexible vs. inflexible consumption, wealth-dependent capital for smart appliances) to achieve coordination. The mandate-atrophy pathway: (1) Original mandate: implement demand response to stabilize grid during peak demand (genuine coordination need). (2) Atrophy: as natural gas peaking plants are retired and battery storage scales, demand response becomes increasingly performative — the grid no longer REQUIRES consumer-side flexibility in the same way. (3) Institutional persistence: rate structures persist because cost recovery mechanisms and regulatory precedent are locked in, not because grid stability requires them. (4) Extraction residue: costs remain distributed asymmetrically because switching to supply-side mechanisms would require reneging on revenue models. The resolution mechanism: Phase demand response out in favor of distribution-side battery deployment, microgrids, and distributed renewable integration. This is feasible within 15-20 years as storage costs decline. Until then, demand response operates as Tangled Rope with clear mandate toward equity protections and measured atrophy metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_demand_flexibility,
    'Can grid stability be achieved through supply-side flexibility (storage, dispatchable renewables) rather than demand-side rate discrimination?',
    'Comparative analysis of grid stability under alternative balancing mechanisms; technical feasibility and cost studies of battery storage scaling vs. demand response scaling',
    'If supply-side flexibility sufficient: demand response is pure extraction (Snare classification), justifying transition to alternative mechanisms. If supply-side alone cannot match demand response cost-effectiveness: demand response is genuine hybrid (Tangled Rope) justifying continued enforcement with equity protections.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_demand_flexibility, empirical, 'Whether supply-side flexibility can replace demand-side rate discrimination for grid stability').

omega_variable(
    equity_protection_feasibility,
    'Can demand response programs be structured to distribute costs and benefits equitably across income levels without losing grid stabilization effectiveness?',
    'Pilot programs with progressive rate structures; analysis of grid stability outcomes under demand response variants with different equity protections; cross-jurisdictional comparison of equity-adjusted vs. standard demand response',
    'If feasible: Snare classification is design failure, not structural inevitability — enables mandate to implement equity protections. If infeasible: Snare classification is correct; demand response must be phased out in favor of alternative grid technologies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_protection_feasibility, empirical, 'Feasibility of equitable demand response rate design').

omega_variable(
    behavioral_response_saturation,
    'What is the maximum demand reduction achievable through rate incentives, and at what point do further rate increases produce no additional flexibility?',
    'Meta-analysis of demand response studies; measurement of elasticity decay curves across different rate levels; identification of saturation thresholds where behavioral response plateaus',
    'If saturation occurs at low extraction levels: demand response can achieve grid stability with moderate rates (Rope). If saturation occurs at high extraction: large cost shifts to inflexible populations are necessary for grid benefit (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_response_saturation, empirical, 'Maximum achievable demand reduction from rate incentives').

omega_variable(
    smart_meter_equity_access,
    'Do smart meter technologies and dynamic pricing enable or restrict equity in demand response participation for low-income households?',
    'Analysis of smart meter deployment equity; study of digital divide effects on demand response enrollment; comparison of participation rates and savings across income quintiles',
    'If smart meters increase equity: technology can reduce cost-shift harm (mitigates Snare). If smart meters concentrate access: technology amplifies extraction (deepens Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(smart_meter_equity_access, empirical, 'Whether smart meter deployment increases or restricts equity in demand response access').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demand_response_cost_shift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drcs_tr_t0, demand_response_cost_shift, theater_ratio, 0, 0.42).
narrative_ontology:measurement(drcs_tr_t5, demand_response_cost_shift, theater_ratio, 5, 0.53).
narrative_ontology:measurement(drcs_tr_t10, demand_response_cost_shift, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(drcs_be_t0, demand_response_cost_shift, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(drcs_be_t5, demand_response_cost_shift, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(drcs_be_t10, demand_response_cost_shift, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demand_response_cost_shift, resource_allocation).
narrative_ontology:affects_constraint(demand_response_cost_shift, renewable_integration_variability).
narrative_ontology:affects_constraint(demand_response_cost_shift, peak_generation_capacity_requirement).

% DUAL FORMULATION NOTE:
% Demand response cost shift is structurally downstream of both renewable integration (which creates peak/off-peak variance) and legacy peak capacity requirements (which motivate demand reduction). These upstream constraints have different ε values (renewable variability is ~0.15 Mountain; peak capacity is ~0.40 Rope). The demand response cost shift represents the institutional arrangement (rate design) through which grid operators attempt to solve these upstream constraints, but in doing so creates a new Tangled Rope constraint with asymmetric cost distribution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(demand_response_cost_shift, powerful, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
