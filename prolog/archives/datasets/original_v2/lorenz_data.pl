% 1. ENTITIES & INTERVALS
entity(lorenz_manifold, system).
entity(strange_attractor, manifold).
entity(trajectory_alpha, sensor_path).
entity(linear_linearization, scaffold).

interval(simulation_run, 0, 100).
interval(bifurcation_event, 0, 20).

% 2. EVENTS & OMEGA VARIABLES
event(e1, initial_condition_deviation, 0, [delta, 10_minus_8]).
event(e2, orbit_flip, 45, [lobe, transition]).

% Omega Variable: Empirical (Missing high-precision state data)
omega_variable(omega_e1, empirical, precision_limit_of_initial_state_measurement).

% Omega Variable: Conceptual (Definition of 'stability' in a chaotic regime)
omega_variable(omega_c1, conceptual, boundary_definition_between_transient_and_attractor).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Constant Dissipation (The Mountain)
constraint_claim(phase_space_contraction, mountain).
constraint_metric(phase_space_contraction, intensity, 1.0).
constraint_metric(phase_space_contraction, suppression_requirement, 0.05).
constraint_metric(phase_space_contraction, snap_back_potential, 0.0).
constraint_metric(phase_space_contraction, extractiveness, 0.01).

% The Fixed Point Unfolding (The Tangled Rope)
constraint_claim(unstable_manifold_rho28, tangled_rope).
constraint_metric(unstable_manifold_rho28, intensity, 0.85).
constraint_metric(unstable_manifold_rho28, suppression_requirement, 0.42).
constraint_metric(unstable_manifold_rho28, snap_back_potential, 0.78).
constraint_metric(unstable_manifold_rho28, extractiveness, 0.55).

% The Singularity at Infinity (The Snare)
constraint_claim(global_divergence_prevention, snare).
constraint_metric(global_divergence_prevention, intensity, 0.99).
constraint_metric(global_divergence_prevention, suppression_requirement, 0.88).
constraint_metric(global_divergence_prevention, snap_back_potential, 0.12).
constraint_metric(global_divergence_prevention, extractiveness, 0.95).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.55 extractiveness) triggers Reform
recommendation(rec_01, topological_renormalization_of_fixed_points).
affects_constraint(rec_01, unstable_manifold_rho28).

% Snare (0.95 extractiveness) triggers Cut/Replace
recommendation(rec_02, implement_scaffold_linearization_to_prevent_divergence).
affects_constraint(rec_02, global_divergence_prevention).

veto_actor(lyapunov_exponent_limit).
veto_exposed(lyapunov_exponent_limit, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.90, 0.10, 0.80, 0.50]).
measurement(100, [0.40, 0.05, 0.95, 0.10]).

% 6. INTENT EVIDENCE
% Alternative: Laminar flow (Discarded due to Rayleigh-Benard heating parameters)
% Beneficiaries: Macro-scale weather modeling consistency
% Power Delta: Butterfly effect (Micro-fluctuation vs Global State)
intent_evidence(topological_mixing, high_delta).
