% ============================================================================
% CONSTRAINT STORY: demand_response_cost_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-08
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   Demand Response (DR) programs are designed to stabilize electrical grids
 *   by incentivizing users to reduce consumption during peak hours. While
 *   benefiting grid operators and some customers, DR programs can also create
 *   cost shifts that negatively impact certain customer segments,
 *   particularly those with low electricity usage or limited ability to
 *   adjust their consumption patterns.
 *
 * KEY AGENTS:
 *   - Grid Operators: Primary beneficiary (institutional/arbitrage) — benefits from reduced peak demand and improved grid stability
 *   - Early Adopting Customers: Secondary beneficiary (powerful/mobile) — benefits from lower bills through adjusted usage
 *   - Late Adopting Customers: Primary victim (moderate/constrained) — costs through inflexible schedules, pays increased rates
 *   - Low Usage Customers: Secondary victim (powerless/trapped) — costs through fixed costs/rates
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(demand_response_cost_shift, 0.55).
domain_priors:suppression_score(demand_response_cost_shift, 0.4).
domain_priors:theater_ratio(demand_response_cost_shift, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(demand_response_cost_shift, extractiveness, 0.55).
narrative_ontology:constraint_metric(demand_response_cost_shift, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(demand_response_cost_shift, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(demand_response_cost_shift, tangled_rope).
narrative_ontology:human_readable(demand_response_cost_shift, "Smart Grid Demand Response Rate Structure").
narrative_ontology:topic_domain(demand_response_cost_shift, "economic/technological").

domain_priors:requires_active_enforcement(demand_response_cost_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, grid_operators).
narrative_ontology:constraint_beneficiary(demand_response_cost_shift, early_adopting_customers).
narrative_ontology:constraint_victim(demand_response_cost_shift, late_adopting_customers).
narrative_ontology:constraint_victim(demand_response_cost_shift, low_usage_customers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Low usage customers (e.g., those with solar panels or minimal electricity needs) find themselves paying a disproportionately higher rate because they don't benefit from the DR incentives and still bear the fixed costs of grid maintenance and DR program implementation.
constraint_indexing:constraint_classification(demand_response_cost_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% Customers who cannot easily adjust their usage patterns, either due to inflexible schedules or lack of smart appliances, are constrained. They partially benefit from a more stable grid, but also bear the costs through increased rates as the load is shifted to off-peak hours. Constrained by their inability to arbitrage and mobile.
constraint_indexing:constraint_classification(demand_response_cost_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% Grid operators benefit from the DR program by reducing peak demand and improving grid stability. They can arbitrage by adjusting resource allocation and leveraging real-time grid conditions. Viewed as coordination from this perspective.
constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Early adopters of smart technologies who can actively manage their electricity consumption can lower their bills, contribute to grid stability, and reap the rewards, mobile by design.
constraint_indexing:constraint_classification(demand_response_cost_shift, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% The observer sees the combined extraction and coordination and enforcement.
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

:- end_tests(demand_response_cost_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness score (0.55) reflects the redistribution of costs from high-usage to low-usage customers. Suppression (0.40) captures the limited exit options for customers who are unable to participate in DR programs due to technological or economic constraints. The theater ratio (0.20) is low, DR programs are more substantive then theatric.
 *
 * PERSPECTIVAL GAP:
 *   Low usage customers experiences the DR as a pure extraction because they cannot respond and are burdened by the fixed costs. Early adopting customers sees this as coordination because they have the agency and exit options to mobilize. The analytical observer is at the highest abstraction and sees enforcement mechanism and a way to allocate resources.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries and victims are determined by their relative increase and decrease in cost. Early adopters benefit through reduced costs and grid operators benefit through higher stability. The directionality logic captures who benefits and bears the cost.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    elasticity_of_demand,
    'How responsive are different customer segments to price signals and incentives for demand response?',
    'Econometric analysis of customer usage data under varying DR program designs.',
    'Determines the effectiveness of DR programs and the magnitude of cost shifts between customer segments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elasticity_of_demand, empirical, 'Customer price elasticity and DR responsiveness').

omega_variable(
    fixed_cost_allocation,
    'How should the fixed costs of grid infrastructure and DR program implementation be allocated across different customer classes?',
    'Cost-benefit analysis of alternative allocation methodologies, considering fairness and economic efficiency.',
    'Determines the extent to which low-usage customers subsidize DR programs and whether cost shifts are equitable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(fixed_cost_allocation, preference, 'Fixed cost allocation methodology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(demand_response_cost_shift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dema_tr_t0, demand_response_cost_shift, theater_ratio, 0, 0.1).
narrative_ontology:measurement(dema_tr_t5, demand_response_cost_shift, theater_ratio, 5, 0.2).
narrative_ontology:measurement(dema_tr_t10, demand_response_cost_shift, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(dema_be_t0, demand_response_cost_shift, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(dema_be_t5, demand_response_cost_shift, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(dema_be_t10, demand_response_cost_shift, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(demand_response_cost_shift, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
