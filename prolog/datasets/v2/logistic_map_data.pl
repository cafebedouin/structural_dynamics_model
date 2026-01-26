% 1. ENTITIES & INTERVALS
entity(logistic_operator, system).
entity(unit_interval, manifold).
entity(orbit_beta, sensor_path).
entity(periodic_window_3, scaffold).

interval(iteration_sequence, 0, 1000).
interval(transient_phase, 0, 50).

% 2. EVENTS & OMEGA VARIABLES
event(e1, bifurcation_point, 0, [parameter_r, 4_0]).
event(e2, sensitivity_manifestation, 12, [divergence, exponential]).

% Omega Variable: Empirical (Uncertainty in floating-point precision)
omega_variable(omega_e1, empirical, rounding_error_propagation_at_high_iteration).

% Omega Variable: Conceptual (Definition of 'equilibrium' in non-convergent states)
omega_variable(omega_c1, conceptual, classification_of_intermittency_as_stochastic_or_deterministic).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Unit Interval Bound (The Mountain)
% The map is strictly mapped [0, 1] -> [0, 1] for r <= 4.
constraint_claim(domain_confinement, mountain).
constraint_metric(domain_confinement, intensity, 1.0).
constraint_metric(domain_confinement, suppression_requirement, 0.05).
constraint_metric(domain_confinement, snap_back_potential, 0.0).
constraint_metric(domain_confinement, extractiveness, 0.01).

% Period Doubling Cascade (The Tangled Rope)
% Structural complexity leading to chaos (r approx 3.57).
constraint_claim(bifurcation_memory, tangled_rope).
constraint_metric(bifurcation_memory, intensity, 0.75).
constraint_metric(bifurcation_memory, suppression_requirement, 0.38).
constraint_metric(bifurcation_memory, snap_back_potential, 0.60).
constraint_metric(bifurcation_memory, extractiveness, 0.52).

% Ergodic Covering at r=4 (The Snare)
% The system maps the entire interval, destroying specific orbital identity.
constraint_claim(total_entropy_saturation, snare).
constraint_metric(total_entropy_saturation, intensity, 0.98).
constraint_metric(total_entropy_saturation, suppression_requirement, 0.85).
constraint_metric(total_entropy_saturation, snap_back_potential, 0.05).
constraint_metric(total_entropy_saturation, extractiveness, 0.92).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.52 extractiveness) triggers Reform
recommendation(rec_01, parameter_tuning_to_stable_periodic_window).
affects_constraint(rec_01, bifurcation_memory).

% Snare (0.92 extractiveness) triggers Cut
% Utilizing the 'periodic_window_3' scaffold to interrupt chaos.
recommendation(rec_02, inject_periodic_scaffold_to_break_ergodicity).
affects_constraint(rec_02, total_entropy_saturation).

veto_actor(shannon_entropy_ceiling).
veto_exposed(shannon_entropy_ceiling, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.95, 0.40, 0.60, 0.80]).
measurement(1000, [0.05, 0.01, 0.99, 0.02]).

% 6. INTENT EVIDENCE
% Alternative: Linear growth model (Discarded: lacks density-dependent feedback)
% Beneficiaries: Population dynamicists modeling resource exhaustion
% Power Delta: Parameter sensitivity (r-value increment vs global topology)
intent_evidence(topological_conjugacy, high_delta).
