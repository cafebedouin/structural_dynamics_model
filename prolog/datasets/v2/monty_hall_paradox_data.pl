% 1. ENTITIES & INTERVALS
entity(game_show_environment, system).
entity(probability_space, manifold).
entity(contestant_choice_path, sensor_path).
entity(host_knowledge_base, scaffold).

interval(game_round, 0, 2).
interval(decision_window, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, initial_door_selection, 0, [choice, door_1]).
event(e2, host_reveal_goat, 1, [revealed_door, door_3]).

% Omega Variable: Conceptual (The 'Monty Hall' assumption: host must open a door with a goat)
omega_variable(omega_c1, conceptual, host_behavior_protocol_invariance).

% Omega Variable: Preference (Psychological bias toward initial choice vs. switching)
omega_variable(omega_p1, preference, cognitive_resistance_to_statistical_reevaluation).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Initial Distribution (The Mountain)
% The car is placed behind one of three doors with equal p=1/3. This setup is invariant.
constraint_claim(uniform_priors, mountain).
constraint_metric(uniform_priors, intensity, 1.0).
constraint_metric(uniform_priors, suppression_requirement, 0.05).
constraint_metric(uniform_priors, snap_back_potential, 0.0).
constraint_metric(uniform_priors, extractiveness, 0.01).

% The Conditional Update (The Tangled Rope)
% Opening a door creates a non-intuitive shift: p(switch)=2/3 vs p(stay)=1/3.
% Extractiveness at 0.60 triggers a Reform recommendation to clarify the state space.
constraint_claim(bayesian_probability_shift, tangled_rope).
constraint_metric(bayesian_probability_shift, intensity, 0.85).
constraint_metric(bayesian_probability_shift, suppression_requirement, 0.40).
constraint_metric(bayesian_probability_shift, snap_back_potential, 0.75).
constraint_metric(bayesian_probability_shift, extractiveness, 0.60).

% The Equivalence Fallacy (The Noose)
% The false belief that the two remaining doors have a 50/50 chance. 
% This "strangles" the rational choice and requires the 'host_knowledge_base' scaffold.
constraint_claim(naive_fifty_fifty_heuristics, noose).
constraint_metric(naive_fifty_fifty_heuristics, intensity, 0.95).
constraint_metric(naive_fifty_fifty_heuristics, suppression_requirement, 0.88).
constraint_metric(naive_fifty_fifty_heuristics, snap_back_potential, 0.10).
constraint_metric(naive_fifty_fifty_heuristics, extractiveness, 0.90).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.60 extractiveness) triggers Reform
recommendation(rec_01, visualize_total_state_space_via_permutation_table).
affects_constraint(rec_01, bayesian_probability_shift).

% Noose (0.90 extractiveness) triggers Cut
% Utilizing 'host_knowledge_base' scaffold to account for the host's non-random action.
recommendation(rec_02, abandon_naive_symmetry_in_favor_of_host_intent_analysis).
affects_constraint(rec_02, naive_fifty_fifty_heuristics).

veto_actor(vos_savant_controversy_limit).
veto_exposed(vos_savant_controversy_limit, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency is max at the switch decision; Stability is low during the reveal.
measurement(0, [1.00, 1.00, 0.33, 1.00]).
measurement(2, [1.00, 0.50, 0.67, 0.90]).

% 6. INTENT EVIDENCE
% Alternative: Random host opening (Discarded: alters the probability to 50/50 if car can be revealed)
% Beneficiaries: Decision theorists and cognitive scientists (studying human intuition errors)
% Power Delta: Knowledge Asymmetry (Host's hidden info vs Contestant's observable data)
intent_evidence(information_gain_via_elimination, high_delta).
