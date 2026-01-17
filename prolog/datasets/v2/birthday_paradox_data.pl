% =============================================================================
% DOMAIN: Birthday Paradox (Combinatorial Collision Probability)
% MISSION: Clinical Sensor Output [v3.1.8]
% =============================================================================

% 1. ENTITIES & INTERVALS
% -----------------------------------------------------------------------------
entity(probability_space, process).
entity(individual_pool, group).
entity(calendar_days, group).
entity(pigeonhole_principle_scaffold, scaffold).
entity(complementary_counting_scaffold, scaffold).

% PRIMARY INTERVAL ONLY: Synchronized with Scenario Manager
interval(calculation_lifespan, 0, 10).

% 2. EVENTS & OMEGA VARIABLES
% -----------------------------------------------------------------------------
% FIXED: Simplified properties to simple atoms to prevent type_errors.
event(population_sampling, initialization, 0, [random_distribution]).
event(pair_comparison, processing, 4, [combinatorial_expansion]).
event(probability_crossover, update, 5, [n_equals_23, p_high]).
event(saturation_event, termination, 10, [n_equals_366]).

% OMEGA CLASSIFICATION
% Ω_E: Empirical (Data requirement)
omega_variable(omega_1, empirical, 'Exact impact of leap years on distribution precision.').

% Ω_C: Conceptual (Definition requirement)
omega_variable(omega_2, conceptual, 'Definition of randomness in non-uniform birth rate datasets.').

% Ω_P: Preference (Stakeholder value choice)
omega_variable(omega_3, preference, 'Utility threshold for acceptable collision risk in hashing.').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% -----------------------------------------------------------------------------

% MOUNTAIN: Discrete Calendar Limits (supp <= 0.05, snap = 0.0)
constraint_claim(pigeonhole_boundary, mountain).
constraint_metric(pigeonhole_boundary, suppression_requirement, 0.05).
constraint_metric(pigeonhole_boundary, snap_back_potential, 0.00).

% ROPE: Independent Probability Norms (ext <= 0.35, supp <= 0.15)
constraint_claim(independence_assumption, rope).
constraint_metric(independence_assumption, extractiveness, 0.28).
constraint_metric(independence_assumption, suppression_requirement, 0.12).

% TANGLED ROPE: Intuition Gap Friction (ext 0.36-0.65)
constraint_claim(human_probabilistic_bias, tangled_rope).
constraint_metric(human_probabilistic_bias, extractiveness, 0.55).
constraint_metric(human_probabilistic_bias, suppression_requirement, 0.35).

% NOOSE: Combinatorial Explosion Obstruction (ext >= 0.66, supp >= 0.46)
constraint_claim(linear_scaling_trap, noose).
constraint_metric(linear_scaling_trap, extractiveness, 0.92).
constraint_metric(linear_scaling_trap, suppression_requirement, 0.85).

% 4. RECOMMENDATIONS & VETO POINTS
% -----------------------------------------------------------------------------
recommendation(rec_03, 'Reform estimation logic via the pigeonhole_principle_scaffold.').
affects_constraint(rec_03, human_probabilistic_bias).

recommendation(rec_04, 'Cut linear scaling errors via the complementary_counting_scaffold.').
affects_constraint(rec_04, linear_scaling_trap).

% VETO LOGIC: Syncing veto_actor with veto_exposed.
veto_actor(non_uniform_birth_dataset).
veto_exposed(non_uniform_birth_dataset, rec_02).

% 5. MEASUREMENTS [Agency, Stability, Utility, Resilience]
% -----------------------------------------------------------------------------
measurement(0, [0.90, 0.40, 0.20, 0.50]).
measurement(10, [0.20, 0.98, 0.95, 0.88]).

% 6. INTENT EVIDENCE (Unified intent_fact/4 Standard)
% -----------------------------------------------------------------------------
intent_fact(viable_alternative, calculation_lifespan, generalized_birthday_problem, 0.70).
intent_fact(viable_alternative, calculation_lifespan, hash_collision_attack, 0.80).
intent_fact(beneficiary, cryptographers, duplicate_mapping, 1.0).
intent_fact(power_change, combinatorial_complexity, linear_intuition, 0.95).

