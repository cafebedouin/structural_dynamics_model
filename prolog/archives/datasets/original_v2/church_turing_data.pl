% 1. ENTITIES & INTERVALS
entity(universal_computer, system).
entity(recursive_function_space, manifold).
entity(computation_trace, sensor_path).
entity(oracle_machine, scaffold).

interval(algorithmic_steps, 0, 10000).
interval(halt_check, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, input_encoding, 0, [alphabet, binary]).
event(e2, state_transition, 42, [operation, write_move]).

% Omega Variable: Conceptual (The unprovable nature of a 'thesis' vs a 'theorem')
omega_variable(omega_c1, conceptual, equivalence_between_intuitive_calculability_and_formal_logic).

% Omega Variable: Empirical (The physical limits of real-world hardware vs infinite tape)
omega_variable(omega_e1, empirical, energy_dissipation_and_thermodynamics_of_erasure).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Logical Boundary (The Mountain)
% The set of functions computable by a Turing machine is fixed; it is a fundamental law of logic.
constraint_claim(turing_completeness, mountain).
constraint_metric(turing_completeness, intensity, 1.0).
constraint_metric(turing_completeness, suppression_requirement, 0.05).
constraint_metric(turing_completeness, snap_back_potential, 0.0).
constraint_metric(turing_completeness, extractiveness, 0.01).

% The Step-Wise Complexity (The Tangled Rope)
% Nested recursions and pointer-chasing create a dense execution graph.
% Extractiveness at 0.48 triggers a Reform recommendation for optimization.
constraint_claim(algorithmic_overhead, tangled_rope).
constraint_metric(algorithmic_overhead, intensity, 0.72).
constraint_metric(algorithmic_overhead, suppression_requirement, 0.32).
constraint_metric(algorithmic_overhead, snap_back_potential, 0.65).
constraint_metric(algorithmic_overhead, extractiveness, 0.48).

% The Halting Problem (The Snare)
% The undecidability of whether an arbitrary program stops. This "strangles" total automation.
% Extractiveness at 0.99 requires an 'oracle_machine' scaffold to hypothesize solutions.
constraint_claim(undecidability_barrier, snare).
constraint_metric(undecidability_barrier, intensity, 0.99).
constraint_metric(undecidability_barrier, suppression_requirement, 0.95).
constraint_metric(undecidability_barrier, snap_back_potential, 0.01).
constraint_metric(undecidability_barrier, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.48 extractiveness) triggers Reform
recommendation(rec_01, transformation_to_combinatory_logic_for_reduction_simplification).
affects_constraint(rec_01, algorithmic_overhead).

% Snare (0.99 extractiveness) triggers Cut
% Utilizing 'oracle_machine' scaffold to bypass the halting limit for specific sub-classes.
recommendation(rec_02, isolate_primitive_recursive_functions_to_ensure_termination).
affects_constraint(rec_02, undecidability_barrier).

veto_actor(godel_incompleteness_limit).
veto_exposed(godel_incompleteness_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (universal capability) is 1.0, but Stability drops when approaching undecidable states.
measurement(0, [1.00, 1.00, 1.00, 1.00]).
measurement(10000, [1.00, 0.15, 0.50, 0.80]).

% 6. INTENT EVIDENCE
% Alternative: Hypercomputation (Discarded: lacks physical realization evidence)
% Beneficiaries: Software architects and complexity theorists
% Power Delta: Effective Procedure (Finitary rules vs Infinite possibility space)
intent_evidence(functional_equivalence, high_delta).
