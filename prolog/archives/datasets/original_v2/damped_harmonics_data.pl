% 1. ENTITIES & INTERVALS
entity(harmonic_oscillator, system).
entity(phase_space_plane, manifold).
entity(decay_trajectory, sensor_path).
entity(critical_damping_threshold, scaffold).

interval(oscillation_period, 0, 50).
interval(settling_time, 0, 100).

% 2. EVENTS & OMEGA VARIABLES
event(e1, initial_displacement, 0, [amplitude, 1_0]).
event(e2, zero_crossing, 12, [velocity, maximum]).

% Omega Variable: Empirical (Friction coefficient fluctuations)
omega_variable(omega_e1, empirical, measurement_of_non_linear_drag_coefficients).

% Omega Variable: Conceptual (Definition of 'rest' in asymptotic decay)
omega_variable(omega_c1, conceptual, threshold_definition_for_system_quiescence).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Hooke Law Restoring Force (The Mountain)
% The fundamental linear return mechanism; must remain invariant.
constraint_claim(restoring_potential, mountain).
constraint_metric(restoring_potential, intensity, 1.0).
constraint_metric(restoring_potential, suppression_requirement, 0.05).
constraint_metric(restoring_potential, snap_back_potential, 0.0).
constraint_metric(restoring_potential, extractiveness, 0.01).

% The Energy Dissipation Envelope (The Tangled Rope)
% The interaction between velocity and friction creates a decaying spiral.
% At r=0.42, it suggests a need for parameter adjustment to reach criticality.
constraint_claim(viscous_damping, tangled_rope).
constraint_metric(viscous_damping, intensity, 0.65).
constraint_metric(viscous_damping, suppression_requirement, 0.25).
constraint_metric(viscous_damping, snap_back_potential, 0.40).
constraint_metric(viscous_damping, extractiveness, 0.42).

% The Overdamped Stasis (The Snare)
% Excessive damping that prevents oscillation entirely, strangling the frequency.
% Extractiveness > 0.66 requires a scaffold (critical_damping_threshold) to resolve.
constraint_claim(overdamped_restriction, snare).
constraint_metric(overdamped_restriction, intensity, 0.90).
constraint_metric(overdamped_restriction, suppression_requirement, 0.70).
constraint_metric(overdamped_restriction, snap_back_potential, 0.05).
constraint_metric(overdamped_restriction, extractiveness, 0.85).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.42 extractiveness) triggers Reform
recommendation(rec_01, tuning_of_damping_coefficient_to_minimize_settling_time).
affects_constraint(rec_01, viscous_damping).

% Snare (0.85 extractiveness) triggers Cut
% Utilizing 'critical_damping_threshold' scaffold to restore optimal return-to-zero.
recommendation(rec_02, reduction_of_viscosity_to_scaffold_limit).
affects_constraint(rec_02, overdamped_restriction).

veto_actor(conservation_of_energy_law).
veto_exposed(conservation_of_energy_law, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [1.00, 0.20, 0.50, 0.90]).
measurement(100, [0.00, 1.00, 0.10, 1.00]).

% 6. INTENT EVIDENCE
% Alternative: Driven oscillation (Discarded: lack of external periodic force)
% Beneficiaries: Mechanical engineers designing vehicle suspension systems
% Power Delta: Damping Ratio (Zeta value vs Decay Rate)
intent_evidence(exponential_decay_envelope, high_delta).
