% ============================================================================
% CONSTRAINT STORY: airport_slot_use_it_or_lose_it
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_airport_slot_use_it_or_lose_it, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: airport_slot_use_it_or_lose_it
 *   human_readable: "Use-it-or-lose-it" rule for airport landing slots
 *   domain: economic
 *
 * SUMMARY:
 *   The "use-it-or-lose-it" rule for airport landing slots aims to promote
 *   efficient allocation of scarce airport resources by requiring airlines to
 *   operate a certain percentage of their allocated slots. This rule creates
 *   winners and losers, potentially leading to both efficient use of airport
 *   capacity and wasteful "ghost flights."
 *
 * KEY AGENTS:
 *   - Incumbent Airlines: Primary targets (powerless/trapped in demand shocks, moderate/constrained otherwise) — face the risk of losing valuable slots.
 *   - Competing Airlines: Primary beneficiaries (powerful/mobile) — gain opportunities to acquire slots.
 *   - Airport Authority: Secondary beneficiary (institutional/arbitrage) — benefits from high airport utilization.
 *   - Passengers: Indirect Beneficiaries (powerless) - increased flight options. 
 *   - Analytical Observer: Assesses efficiency (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(airport_slot_use_it_or_lose_it, 0.55).
domain_priors:suppression_score(airport_slot_use_it_or_lose_it, 0.65).
domain_priors:theater_ratio(airport_slot_use_it_or_lose_it, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, extractiveness, 0.55).
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(airport_slot_use_it_or_lose_it, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(airport_slot_use_it_or_lose_it, tangled_rope).
narrative_ontology:human_readable(airport_slot_use_it_or_lose_it, "\"Use-it-or-lose-it\" rule for airport landing slots").
narrative_ontology:topic_domain(airport_slot_use_it_or_lose_it, "economic").

domain_priors:requires_active_enforcement(airport_slot_use_it_or_lose_it).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, competing_airlines).
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, airport_authority).
narrative_ontology:constraint_beneficiary(airport_slot_use_it_or_lose_it, passengers).
narrative_ontology:constraint_victim(airport_slot_use_it_or_lose_it, incumbent_airlines).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Incumbent airlines that experience a sudden and significant drop in demand (e.g., due to a recession, pandemic, or major event affecting travel to a specific location) are trapped by the rule. They must continue to operate flights even when unprofitable to avoid losing valuable slots.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% Incumbent airlines may find ways to use slots for less profitable routes or 'ghost flights' to retain slots, but this represents an inefficient allocation of resources. The rule also provides some coordination function in maintaining airport operational integrity.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% The airport authority benefits from the rule as it ensures high utilization of airport infrastructure and can reallocate slots to more efficient or growing airlines, thereby maximizing airport revenue. This is experienced as a coordination mechanism.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% Competing airlines that desire access to congested airports benefit from the rule as it creates opportunities to acquire slots from incumbent airlines that cannot meet the utilization requirements. Airlines might have mobile assets to allocate.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% From an analytical perspective, the rule is a mixed bag. It addresses the coordination problem of efficiently allocating scarce airport resources but also introduces distortions and inefficiencies due to the pressure to maintain slot utilization even when demand is low.
constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(airport_slot_use_it_or_lose_it_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(airport_slot_use_it_or_lose_it, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(airport_slot_use_it_or_lose_it, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(airport_slot_use_it_or_lose_it_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The rule extracts value from incumbent airlines by threatening to take away their slots if they don't use them (extractiveness 0.55). It suppresses alternative uses of capital, because airlines can't simply choose to not fly to a certain destination, or at least have that choice be much more costly (suppression 0.65). It has a low theater ratio (0.30), meaning the flights occurring are at least somewhat functional.
 *
 * PERSPECTIVAL GAP:
 *   Incumbent airlines see the rule as a potential snare because they must operate flights even when unprofitable to retain slots. Competing airlines and the airport authority see it as a means to ensure efficient use of airport capacity. The analytical observer acknowledges both the benefits and drawbacks, hence tangled rope classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Incumbent airlines are victims because they face the risk of losing slots. Competing airlines and the airport authority benefit from the rule. The directionality reflects these structural relationships.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    demand_threshold,
    'What is the appropriate threshold for defining a significant drop in demand that warrants an exemption from the rule?',
    'Statistical analysis of historical demand fluctuations and their impact on airline profitability.',
    'Too low: Airlines may exploit exemptions for strategic purposes. Too high: Airlines face undue pressure to operate unprofitable flights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demand_threshold, empirical, 'Demand threshold for exemptions.').

omega_variable(
    slot_valuation,
    'What is the true economic value of airport slots, considering both direct revenue and indirect benefits (e.g., network effects)?',
    'Econometric modeling of airline revenue, network connectivity, and airport traffic patterns.',
    'Underestimated: Rule may lead to inefficient reallocation of slots. Overestimated: Rule may create excessive pressure to maintain slot utilization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(slot_valuation, empirical, 'Economic value of slots.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(airport_slot_use_it_or_lose_it, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airp_tr_t0, airport_slot_use_it_or_lose_it, theater_ratio, 0, 0.1).
narrative_ontology:measurement(airp_tr_t5, airport_slot_use_it_or_lose_it, theater_ratio, 5, 0.2).
narrative_ontology:measurement(airp_tr_t10, airport_slot_use_it_or_lose_it, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(airp_be_t0, airport_slot_use_it_or_lose_it, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(airp_be_t5, airport_slot_use_it_or_lose_it, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(airp_be_t10, airport_slot_use_it_or_lose_it, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(airport_slot_use_it_or_lose_it, resource_allocation).
narrative_ontology:affects_constraint(airport_slot_use_it_or_lose_it, airline_competition).
narrative_ontology:affects_constraint(airport_slot_use_it_or_lose_it, airport_capacity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
