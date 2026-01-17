% 1. ENTITIES & INTERVALS
entity(geometric_probability_engine, system).
entity(euclidean_plane_strips, manifold).
entity(needle_drop_trajectory, sensor_path).
entity(monte_carlo_estimator, scaffold).

interval(trial_sequence, 0, 10000).
interval(integration_window, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, initial_drop, 1, [distance_x, theta]).
event(e2, crossing_verification, 1, [result, intersection_detected]).

% Omega Variable: Conceptual (The transition from physical drops to abstract integration)
omega_variable(omega_c1, conceptual, validity_of_stochastic_sampling_as_mathematical_proof).

% Omega Variable: Empirical (Precision limits of the physical needle's center point and angle)
omega_variable(omega_e1, empirical, measurement_error_in_theta_resolution).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Parallel Strip Uniformity (The Mountain)
% The lines are spaced exactly at distance 't'; the geometry of the plane is invariant.
constraint_claim(strip_parallelism, mountain).
constraint_metric(strip_parallelism, intensity, 1.0).
constraint_metric(strip_parallelism, suppression_requirement, 0.05).
constraint_metric(strip_parallelism, snap_back_potential, 0.0).
constraint_metric(strip_parallelism, extractiveness, 0.01).

% The Trigonometric Dependency (The Tangled Rope)
% The crossing condition $x \le (l/2) \sin(\theta)$ entangles the spatial position with the orientation.
% Extractiveness at 0.48 triggers a Reform recommendation for coordinate simplification.
constraint_claim(angular_spatial_coupling, tangled_rope).
constraint_metric(angular_spatial_coupling, intensity, 0.72).
constraint_metric(angular_spatial_coupling, suppression_requirement, 0.32).
constraint_metric(angular_spatial_coupling, snap_back_potential, 0.55).
constraint_metric(angular_spatial_coupling, extractiveness, 0.48).

% The Infinite Precision Requirement (The Noose)
% To recover $\pi$, an infinite number of drops or infinite measurement precision is required.
% This "strangles" the physical observer and requires the 'monte_carlo_estimator' scaffold.
constraint_claim(irrational_convergence_limit, noose).
constraint_metric(irrational_convergence_limit, intensity, 0.96).
constraint_metric(irrational_convergence_limit, suppression_requirement, 0.90).
constraint_metric(irrational_convergence_limit, snap_back_potential, 0.02).
constraint_metric(irrational_convergence_limit, extractiveness, 0.94).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.48 extractiveness) triggers Reform
recommendation(rec_01, transform_to_integral_representation_over_half_period).
affects_constraint(rec_01, angular_spatial_coupling).

% Noose (0.94 extractiveness) triggers Cut
% Utilizing 'monte_carlo_estimator' scaffold to approximate rather than solve.
recommendation(rec_02, discard_physical_measurement_for_computational_simulation).
affects_constraint(rec_02, irrational_convergence_limit).

veto_actor(law_of_large_numbers).
veto_exposed(law_of_large_numbers, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (ability to drop) is high; Utility for Pi estimation grows with trial count.
measurement(0, [1.00, 0.20, 0.05, 0.80]).
measurement(10000, [1.00, 0.95, 0.90, 0.99]).

% 6. INTENT EVIDENCE
% Alternative: Buffon's Noodle (Discarded: curvature complicates the linear intersection check)
% Beneficiaries: Early probabilists and modern Monte Carlo simulation designers
% Power Delta: Transcendental Recovery (Physical movement vs Transcendental constant Pi)
intent_evidence(geometric_probability_emergence, high_delta).
