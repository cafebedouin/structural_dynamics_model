% ============================================================================
% CONSTRAINT STORY: demand_response_cost_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   domain: economic/technological/energy
 *
 * SUMMARY:
 *   Demand response programs were initially designed as pure coordination
 *   mechanisms: incentivize users to reduce electricity consumption during
 *   peak hours, thereby avoiding the need to build expensive generation and
 *   transmission capacity while enabling higher renewable energy penetration.
 *   However, the policy implementation creates asymmetric cost burdens.
 *   Utilities implement time-of-use rates and critical peak pricing that
 *   penalize households and small businesses lacking capital to invest in
 *   smart devices, battery storage, or load-shifting infrastructure.
 *   Meanwhile, large industrial users and facilities with sophisticated load
 *   management benefit from rebates and favorable rate structures. The result
 *   is a hybrid constraint: genuine grid stabilization function
 *   (coordination) coupled with systematic cost shifting from
 *   price-sensitive, less-mobile consumers to grid operators and capital-rich
 *   industrial users (extraction). The constraint operates through rate
 *   structure design and regulatory mandate, not through deception.
 *   Households know the peak hour rates. But the trapped position of
 *   price-sensitive consumers creates asymmetric vulnerability: they cannot
 *   exit the grid, cannot afford demand response technology, and face binary
 *   choices between consumption curtailment and financial burden.
 *
 * KEY AGENTS:
 *   - Price-Sensitive Households: Primary victim (powerless/trapped) — no flexibility, no technology access, no exit option; bear disproportionate cost of grid stabilization
 *   - Small Businesses: Secondary victim (moderate/constrained) — limited capital for smart load management, regulatory participation mandates, constrained flexibility relative to industrial users
 *   - Grid Operators & Utilities: Primary beneficiary (institutional/arbitrage) — avoid expensive capacity investments, reduce operational costs, gain flexibility; arbitrage exit options
 *   - Peak-Load Industrial Users: Secondary beneficiary (powerful/arbitrage) — flexible production, capital for smart technology, derive rebate benefits; mobile across rate structures and locations
 *   - Renewable Integration Coalition: Transitional actor (organized/constrained) — view demand response as temporary necessity for high renewable penetration; building alternatives (storage, distributed generation)
 *   - Legacy Regulatory Framework: Institutional actor (institutional/arbitrage) — maintains rate structure through regulatory inertia; theater-heavy because actual peak reduction effectiveness is modest relative to monitoring/compliance costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demand_response_cost_shift, 0.52).
domain_priors:suppression_score(demand_response_cost_shift, 0.58).
domain_priors:theater_ratio(demand_response_cost_shift, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demand_response_cost_shift, extractiveness, 0.52).
narrative_ontology:constraint_metric(demand_response_cost_shift, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(demand_response_cost_shift, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demand_response_cost_shift, tangled_rope).
narrative_ontology:human_readable(demand_response_cost_shift, "Smart Grid Demand Response Rate Structure").
narrative_ontology:topic_domain(demand_response_cost_shift, "economic/technological/energy").

domain_priors:requires_active_enforcement(demand_response_cost_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, grid_operators).
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, peak_load_industrial_users).
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, utility_shareholders).
narrative_ontology:constraint_victim(demand_response_cost_shift, price_sensitive_households).
narrative_ontology:constraint_victim(demand_response_cost_shift, small_businesses).
narrative_ontology:constraint_victim(demand_response_cost_shift, grid_reliability_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRICE-SENSITIVE HOUSEHOLD (SNARE) — Cannot exit the grid. Faces binary choice: curtail discretionary consumption during peak hours or pay peak rates. No real flexibility for essential loads (heating/cooling, refrigeration). No arbitrage option — trapped into the rate structure with zero degrees of freedom. Experiences maximum extractiveness.
constraint_indexing:constraint_classification(demand_response_cost_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS (TANGLED ROPE) — Constrained by regulatory participation mandates and limited capital for load-shifting investments (battery storage, HVAC scheduling). Benefits from grid stability that enables their operations. But extraction is real: demand response programs often shift costs to smaller firms that lack sophisticated load management. Moderate power, constrained exit.
constraint_indexing:constraint_classification(demand_response_cost_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: GRID OPERATORS & UTILITIES (ROPE) — Experience demand response as pure coordination mechanism: stabilize peak loads without building expensive generation or transmission capacity. Benefits from lower capital requirements and operational flexibility. Arbitrage exit option (can adjust rate structure, expand demand response, or build capacity if needed). Net beneficiary, but classify as rope because the primary function is genuine coordination, not pure extraction.
constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PEAK-LOAD INDUSTRIAL USERS (ROPE) — Large manufacturers with flexible production schedules and capital to invest in demand response technology (automated load shifting, on-site generation). Derive significant economic benefit from peak hour rebates. Have arbitrage options: relocate production, invest in generation, adjust schedules. Coordinate with grid operators to mutual benefit.
constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: RENEWABLE INTEGRATION COALITION (SCAFFOLD) — Organized agents (environmental regulators, renewable operators, grid modernization advocates) view demand response as a temporary coordination mechanism with a sunset clause. As distributed solar, battery storage, and grid-scale storage mature, demand response's coordination function becomes less critical. Low theater — actual grid stability function is real. But temporary: within 15-20 years, storage and microgrids reduce dependence on curtailment-based demand response. Constrained because investment in demand response infrastructure is now required for transition.
constraint_indexing:constraint_classification(demand_response_cost_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: LEGACY REGULATORY FRAMEWORK (PITON) — The rate-structure theater of demand response: the existing framework (time-of-use rates, critical peak pricing, etc.) persists through regulatory inertia. The framework was innovative 15 years ago but is increasingly performative as it fails to address deeper problems (grid stability, equity, decarbonization). Maintained because alternatives haven't fully replaced it, not because it functions optimally. Theater ratio high because compliance and measurement activity dominates actual demand shifting effectiveness.
constraint_indexing:constraint_classification(demand_response_cost_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Demand response exhibits genuine coordination function (reducing peak loads, enabling renewable integration) AND asymmetric cost shifting (to price-sensitive households, small businesses, communities without capital for smart devices). Active enforcement through regulatory mandates and tariff structures confirms the tangled nature. This is not a mountain (immutable law) nor a pure rope (coordination without extraction). The structure is contingent on rate design choices that could be reformed to reduce the victim burden.
constraint_indexing:constraint_classification(demand_response_cost_shift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.52): Moderate-high. The primary extraction is the systematic cost shift from price-sensitive households to utilities and industrial users. The measurement tracks this increase over the interval: as demand response programs mature, utilities layer additional complexity (critical peak pricing, behavioral demand response, real-time pricing) that increases the coordination function but also the administrative overhead and cost shifting. From the trapped household perspective, extractiveness is near 1.0. From the industrial beneficiary perspective, it approaches 0.0. The average of 0.52 reflects that significant populations experience genuine extraction while others experience genuine coordination benefit. Suppression (0.58): Moderate-high. The suppression operates through regulatory mandate (participation required in many jurisdictions), technological barriers (smart devices, real-time pricing capability), and informational asymmetry (households often unclear on actual peak hours or peak rate magnitude). Crucially, suppression is not coercive violence but structural: trapped position in grid + no arbitrage technology + rate structure design. Theater ratio (0.61): Moderate-high. Significant performative content: demand response effectiveness is often overestimated because it measures voluntary curtailment during called events (typically <40 hours/year) rather than actual peak reduction. Utilities report demand response 'capacity' in their marketing but actual delivered response varies widely. Regulatory compliance reporting creates theater—sophisticated metering and monitoring infrastructure is deployed to measure curtailment, but the actual grid benefit is modest relative to the infrastructure cost. As smart grid technology matures, theater should decrease (automated response, real-time data), but regulatory inertia keeps it high.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival divergence driven by exit options and structural position. The powerless household trapped in the grid and the institutional utility with arbitrage options experience fundamentally different constraints—snare vs. rope—from the same rate structure. The gap is not due to measurement ambiguity or subjective framing but to structural asymmetry in exit capacity. A household cannot exit electricity; a utility can adjust rates or capacity strategy. Small businesses occupy the middle ground (constrained exit) and thus the middle classification (tangled rope). The renewable integration coalition adds a temporal dimension: they see the constraint as temporary (scaffold) because storage and distributed generation create a sunset clause that will make demand response less critical. The legacy regulatory framework appears as piton to the civilizational observer because it's maintaining a ritual (rate structure, monitoring, compliance) whose original function (avoiding expensive generation) is being superceded by cheaper alternatives. The analytical observer's tangled rope classification is the 'view from nowhere': both coordination function and extraction are real, both are necessary given current technology, but both are contingent on policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by the agent's position in the extraction flow. Grid operators and utilities are beneficiaries with arbitrage options (they can adjust rate structures, build generation, or exit demand response programs): d ≈ 0.05-0.15, derived from beneficiary status + arbitrage exit → low d → low/negative f(d) → low experienced chi. Price-sensitive households are victims with trapped exit (they cannot leave the grid, cannot afford alternative supply, cannot shift essential loads): d ≈ 0.90-0.95, derived from victim status + trapped exit → high d → high f(d) → high experienced chi. Small businesses are victims with constrained exit (they can adjust operations but face capital and regulatory barriers): d ≈ 0.65-0.75. Peak-load industrial users are beneficiaries with mobile exit options (they can relocate production or invest in generation): d ≈ 0.20-0.30. The analytical observer's directionality is derived from their structural position as a neutral analyst (d ≈ 0.72-0.73 canonical), which places them outside the extraction flow but able to observe it completely. The engine computes these automatically from beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: Demand response cost shift resolves the false positive trap by distinguishing genuine coordination (grid stabilization, renewable integration) from asymmetric extraction (cost shifting to immobile consumers) within a single structure. The constraint is NOT a snare disguised as coordination (false negative); it is BOTH coordination AND extraction simultaneously, which is the definition of tangled rope. The mandatrophy appears when asking: 'Is demand response good policy (pure rope) or exploitative (snare)?' The answer is both. The analytical perspective correctly identifies tangled rope because (1) demand response has a genuine coordination function—it stabilizes grids and enables renewables, which is not fictional or theater; (2) demand response has asymmetric extraction—cost and burden are systematically shifted to those least able to absorb them; (3) the constraint requires active enforcement through regulatory mandate and rate structure design, confirming that it is not a natural coordination outcome. The mandatrophy is resolved by recognizing that coordination and extraction are not opposites but can be co-present. The policy question is not 'Is this coordination or extraction?' but 'How should we redesign the rate structure to preserve coordination function while reducing extraction?' This requires changing the beneficiary/victim relationship, not declaring the constraint misclassified.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    flexibility_distribution_threshold,
    'What percentage of total addressable load must be shiftable without harm to cross the threshold from extraction-dominant (Snare) to coordination-dominant (Rope)?',
    'Empirical measurement of actual load flexibility by household type, sector, and climate zone; identification of non-shiftable loads (heating/cooling, medical, refrigeration); sensitivity analysis on rate structure design',
    'If threshold < 30%: current demand response inherently extracts from immobile load-holders. If threshold > 60%: demand response could be redesigned as net-neutral coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(flexibility_distribution_threshold, empirical, 'Load flexibility threshold for coordination-dominant classification').

omega_variable(
    cost_shift_magnitude,
    'How much of the grid cost savings from demand response is genuinely accrued to the system vs. shifted to price-sensitive households through higher base rates?',
    'Decomposition of utility cost structures; tracking of savings from avoided peak generation, transmission, and capacity investment vs. changes in base rates and fixed charges; comparison across utilities with different DR program designs',
    'If shift > 70%: primary function is extraction disguised as coordination. If shift < 30%: legitimate coordination with minimal victim burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_shift_magnitude, empirical, 'Proportion of cost savings shifted to captive consumers').

omega_variable(
    technology_equity_timeline,
    'How long until smart demand response technology (connected devices, automated control, real-time pricing) diffuses to >80% of households at costs <$500?',
    'Tracking of device adoption, cost curves, and government subsidy programs; projections from renewable energy system cost learning curves; historical technology diffusion models',
    'If < 10 years: extraction is temporary (scaffold classification strengthened). If > 30 years: extraction persists long-term for significant population (snare classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technology_equity_timeline, empirical, 'Timeline for cost-effective demand response technology access').

omega_variable(
    renewable_storage_substitutability,
    'Can grid-scale battery storage and distributed solar + storage fully replace demand response for grid stabilization within the projected energy transition?',
    'Comparative techno-economic modeling of different grid stabilization strategies; cost curves for batteries, solar, and grid modernization; analysis of demand response irreplaceability under high renewable penetration scenarios',
    'If fully substitutable: demand response is purely temporary (scaffold). If partially required: some tangled rope persists even post-transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_storage_substitutability, empirical, 'Whether storage technology can eliminate demand response necessity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demand_response_cost_shift, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(drcs_tr_t0, demand_response_cost_shift, theater_ratio, 0, 0.35).
narrative_ontology:measurement(drcs_tr_t8, demand_response_cost_shift, theater_ratio, 8, 0.48).
narrative_ontology:measurement(drcs_tr_t16, demand_response_cost_shift, theater_ratio, 16, 0.61).

% Extraction over time
narrative_ontology:measurement(drcs_be_t0, demand_response_cost_shift, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(drcs_be_t8, demand_response_cost_shift, base_extractiveness, 8, 0.4).
narrative_ontology:measurement(drcs_be_t16, demand_response_cost_shift, base_extractiveness, 16, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demand_response_cost_shift, resource_allocation).
narrative_ontology:affects_constraint(demand_response_cost_shift, electricity_grid_reliability).
narrative_ontology:affects_constraint(demand_response_cost_shift, renewable_energy_integration).
narrative_ontology:affects_constraint(demand_response_cost_shift, utility_cost_recovery).

% DUAL FORMULATION NOTE:
% Demand response cost shift is downstream of the renewable energy integration constraint and the grid reliability constraint. These upstream constraints create demand for demand response as a coordination mechanism; the cost shift constraint represents the policy implementation choices that transform that demand into asymmetric extraction. The three constraints form a family where demand response is the bridge between grid technical requirements and their distributional consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(demand_response_cost_shift, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
