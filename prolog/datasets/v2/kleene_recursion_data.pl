% 1. ENTITIES & INTERVALS
entity(recursion_operator, system).
entity(index_space_omega, manifold).
entity(transformation_trace, sensor_path).
entity(smn_enumerator, scaffold).

interval(index_search, 0, 5000).
interval(fixed_point_verification, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, transformation_definition, 0, [function, h_of_x]).
event(e2, fixed_point_convergence, 1, [index_e, index_h_e]).

% Omega Variable: Conceptual (The distinction between the program's index and its computational behavior)
omega_variable(omega_c1, conceptual, interpretation_of_self_reference_without_infinite_regress).

% Omega Variable: Empirical (The physical limits of addressable memory when calculating large indices)
omega_variable(omega_e1, empirical, bit_width_limitations_on_representing_recursive_indices).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Universal Indexing Law (The Mountain)
% The existence of an enumerator for all partial recursive functions is a mathematical constant.
constraint_claim(universal_enumeration, mountain).
constraint_metric(universal_enumeration, intensity, 1.0).
constraint_metric(universal_enumeration, suppression_requirement, 0.05).
constraint_metric(universal_enumeration, snap_back_potential, 0.0).
constraint_metric(universal_enumeration, extractiveness, 0.01).

% The s-m-n Parameterization (The Tangled Rope)
% The complexity of shifting arguments between data and code via the s-m-n theorem.
% Extractiveness at 0.42 triggers a Reform recommendation for cleaner mapping.
constraint_claim(parameter_substitution, tangled_rope).
constraint_metric(parameter_substitution, intensity, 0.70).
constraint_metric(parameter_substitution, suppression_requirement, 0.28).
constraint_metric(parameter_substitution, snap_back_potential, 0.55).
constraint_metric(parameter_substitution, extractiveness, 0.42).

% The Total Function Mirage (The Noose)
% The requirement for the transformation to be a total computable function; if not, the system fails.
% Extractiveness at 0.75 requires the 'smn_enumerator' scaffold to maintain structural integrity.
constraint_claim(totality_constraint, noose).
constraint_metric(totality_constraint, intensity, 0.92).
constraint_metric(totality_constraint, suppression_requirement, 0.65).
constraint_metric(totality_constraint, snap_back_potential, 0.15).
constraint_metric(totality_constraint, extractiveness, 0.75).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.42 extractiveness) triggers Reform
recommendation(rec_01, optimize_smn_indices_via_currying_logic).
affects_constraint(rec_01, parameter_substitution).

% Noose (0.75 extractiveness) triggers Cut
% Utilizing 'smn_enumerator' scaffold to ensure the resulting index is always effectively calculable.
recommendation(rec_02, implement_strict_typing_to_enforce_transformation_totality).
affects_constraint(rec_02, totality_constraint).

veto_actor(rogers_equivalence_theorem).
veto_exposed(rogers_equivalence_theorem, rec_01).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency is absolute (1.0) because the theorem guarantees existence, but Stability fluctuates during search.
measurement(0, [1.00, 0.40, 0.85, 0.90]).
measurement(5000, [1.00, 1.00, 1.00, 1.00]).

% 6. INTENT EVIDENCE
% Alternative: Fixed-point combinator in Lambda calculus (Discarded: domain shift to indices)
% Beneficiaries: Language designers and theorists of self-modifying code
% Power Delta: Meta-Circular Evaluation (Program as data vs Program as actor)
intent_evidence(self_referential_completeness, high_delta).
