% 1. ENTITIES & INTERVALS
entity(universal_turing_machine, system).
entity(parameterized_index_space, manifold).
entity(substitution_trace, sensor_path).
entity(currying_primitive, scaffold).

interval(computation_sequence, 0, 100).
interval(index_derivation, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, argument_fixation, 0, [fixed_values, m_constants]).
event(e2, partial_application, 1, [result_index, s_m_n_result]).

% Omega Variable: Conceptual (Ambiguity in the computational cost of the S-m-n mapping function itself)
omega_variable(omega_c1, conceptual, complexity_class_overhead_of_the_primitive_recursive_s_function).

% Omega Variable: Empirical (Hardware address space limits for generated sub-program indices)
omega_variable(omega_e1, empirical, physical_integer_overflow_risk_in_nested_parameterization).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Effective Computability of S (The Mountain)
% The S-m-n function must be a total computable function; this is a non-negotiable axiom.
constraint_claim(s_function_totality, mountain).
constraint_metric(s_function_totality, intensity, 1.0).
constraint_metric(s_function_totality, suppression_requirement, 0.05).
constraint_metric(s_function_totality, snap_back_potential, 0.0).
constraint_metric(s_function_totality, extractiveness, 0.01).

% The Argument Decoupling (The Tangled Rope)
% The logic required to move m variables from the input stream into the program definition.
% Extractiveness at 0.45 triggers a Reform recommendation for cleaner currying.
constraint_claim(parameter_binding_logic, tangled_rope).
constraint_metric(parameter_binding_logic, intensity, 0.68).
constraint_metric(parameter_binding_logic, suppression_requirement, 0.30).
constraint_metric(parameter_binding_logic, snap_back_potential, 0.50).
constraint_metric(parameter_binding_logic, extractiveness, 0.45).

% The Hardcoding Hazard (The Snare)
% Attempting to hardcode specific values into the machine description without the S-m-n abstraction.
% This "strangles" the ability to generate programs dynamically.
% Extractiveness at 0.78 requires the 'currying_primitive' scaffold to resolve.
constraint_claim(static_index_rigidity, snare).
constraint_metric(static_index_rigidity, intensity, 0.94).
constraint_metric(static_index_rigidity, suppression_requirement, 0.60).
constraint_metric(static_index_rigidity, snap_back_potential, 0.20).
constraint_metric(static_index_rigidity, extractiveness, 0.78).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.45 extractiveness) triggers Reform
recommendation(rec_01, implement_curried_functional_interfaces_to_minimize_s_overhead).
affects_constraint(rec_01, parameter_binding_logic).

% Snare (0.78 extractiveness) triggers Cut
% Utilizing 'currying_primitive' scaffold to enable dynamic partial evaluation.
recommendation(rec_02, replace_static_sub_routines_with_s_m_n_parameterization).
affects_constraint(rec_02, static_index_rigidity).

veto_actor(church_turing_equivalence_limit).
veto_exposed(church_turing_equivalence_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency is high due to partial evaluation power; Stability is high as the logic is primitive recursive.
measurement(0, [0.70, 1.00, 0.80, 0.90]).
measurement(100, [0.95, 1.00, 1.00, 1.00]).

% 6. INTENT EVIDENCE
% Alternative: Manual code rewriting (Discarded: not effectively calculable in general)
% Beneficiaries: Compiler designers (Specialization) and Kleene (Recursion Theorem proof)
% Power Delta: Binding Strength (Free variables vs Bound constants)
intent_evidence(functional_specialization, high_delta).
