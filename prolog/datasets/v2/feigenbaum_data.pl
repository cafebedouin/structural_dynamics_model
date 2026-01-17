% 1. ENTITIES & INTERVALS
entity(universality_class, system).
entity(renormalization_operator, manifold).
entity(accumulation_point, sensor_path).
entity(superstable_orbit, scaffold).

interval(bifurcation_sequence, 0, 256).
interval(scaling_window, 0, 10).

% 2. EVENTS & OMEGA VARIABLES
event(e1, quadratic_maximum_verification, 0, [slope, zero]).
event(e2, renormalization_fixed_point, 5, [scaling_factor, 2_5029]).

% Omega Variable: Empirical (Precision of bifurcation parameter measurement)
omega_variable(omega_e1, empirical, numerical_precision_limit_for_high_order_bifurcations).

% Omega Variable: Conceptual (Defining the boundary of 'universality')
omega_variable(omega_c1, conceptual, extension_of_constants_to_non_quadratic_maxima).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Unimodal Constraint (The Mountain)
% Required for universality; strictly enforces the 'one-hump' topology.
constraint_claim(unimodal_topology, mountain).
constraint_metric(unimodal_topology, intensity, 1.0).
constraint_metric(unimodal_topology, suppression_requirement, 0.05).
constraint_metric(unimodal_topology, snap_back_potential, 0.0).
constraint_metric(unimodal_topology, extractiveness, 0.02).

% The Geometric Scaling (The Tangled Rope)
% The ratio delta (4.669) creates a dense, self-similar entanglement of intervals.
constraint_claim(period_doubling_cascade, tangled_rope).
constraint_metric(period_doubling_cascade, intensity, 0.78).
constraint_metric(period_doubling_cascade, suppression_requirement, 0.35).
constraint_metric(period_doubling_cascade, snap_back_potential, 0.85).
constraint_metric(period_doubling_cascade, extractiveness, 0.48).

% The Accumulation Horizon (The Noose)
% The point where the period becomes infinite, effectively ending periodic governance.
constraint_claim(chaos_accumulation_limit, noose).
constraint_metric(chaos_accumulation_limit, intensity, 0.95).
constraint_metric(chaos_accumulation_limit, suppression_requirement, 0.52).
constraint_metric(chaos_accumulation_limit, snap_back_potential, 0.10).
constraint_metric(chaos_accumulation_limit, extractiveness, 0.72).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.48 extractiveness) triggers Reform
recommendation(rec_01, coordinate_rescaling_via_feigenbaum_alpha).
affects_constraint(rec_01, period_doubling_cascade).

% Noose (0.72 extractiveness) triggers Cut
% Utilizing 'superstable_orbit' scaffold to provide a structural reference point.
recommendation(rec_02, decouple_from_infinite_period_limit_using_superstable_seeds).
affects_constraint(rec_02, chaos_accumulation_limit).

veto_actor(schwarzian_derivative_constraint).
veto_exposed(schwarzian_derivative_constraint, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.90, 0.50, 0.95]).
measurement(256, [0.15, 0.02, 0.98, 0.05]).

% 6. INTENT EVIDENCE
% Alternative: Quasi-periodic route to chaos (Discarded: lacks universal delta constant)
% Beneficiaries: Fluid dynamicists and electronic circuit designers
% Power Delta: Scale Invariance (Local renormalization vs Global bifurcation map)
intent_evidence(functional_fixed_point, high_delta).
