% 1. ENTITIES & INTERVALS
entity(self_replicator, system).
entity(fixed_point_space, manifold).
entity(source_code_stream, sensor_path).
entity(data_code_bridge, scaffold).

interval(execution_cycle, 0, 1).
interval(output_verification, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, string_specification, 0, [content, code_template]).
event(e2, replication_output, 1, [match_ratio, 1_0]).

% Omega Variable: Conceptual (The distinction between 'use' and 'mention' in code)
omega_variable(omega_c1, conceptual, use_mention_distinction_in_self_referential_strings).

% Omega Variable: Empirical (The limitation of physical memory for infinite recursive expansion)
omega_variable(omega_e1, empirical, hardware_precision_limits_on_reproducing_metadata).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Fixed-Point Constraint (The Mountain)
% Kleene's Theorem ensures every computable function has a fixed point; Quines are logically inevitable.
constraint_claim(fixed_point_existence, mountain).
constraint_metric(fixed_point_existence, intensity, 1.0).
constraint_metric(fixed_point_existence, suppression_requirement, 0.05).
constraint_metric(fixed_point_existence, snap_back_potential, 0.0).
constraint_metric(fixed_point_existence, extractiveness, 0.01).

% The Quoting Complexity (The Tangled Rope)
% Managing escape characters and the double-printing logic creates a "tangled" source-to-data mapping.
% Extractiveness at 0.55 triggers a Reform recommendation for serialization.
constraint_claim(quoting_mechanism, tangled_rope).
constraint_metric(quoting_mechanism, intensity, 0.82).
constraint_metric(quoting_mechanism, suppression_requirement, 0.40).
constraint_metric(quoting_mechanism, snap_back_potential, 0.75).
constraint_metric(quoting_mechanism, extractiveness, 0.55).

% The External File Dependency (The Snare)
% A program that simply reads its own source file is not a Quine; it "strangles" the pure self-reference.
% Extractiveness at 0.90 requires the 'data_code_bridge' scaffold to resolve internally.
constraint_claim(io_dependency_exclusion, snare).
constraint_metric(io_dependency_exclusion, intensity, 0.95).
constraint_metric(io_dependency_exclusion, suppression_requirement, 0.88).
constraint_metric(io_dependency_exclusion, snap_back_potential, 0.05).
constraint_metric(io_dependency_exclusion, extractiveness, 0.90).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.55 extractiveness) triggers Reform
recommendation(rec_01, utilize_schematic_repr_to_decouple_logic_from_literal_strings).
affects_constraint(rec_01, quoting_mechanism).

% Snare (0.90 extractiveness) triggers Cut
% Utilizing 'data_code_bridge' to internalize the source template.
recommendation(rec_02, cut_filesystem_calls_to_enforce_internal_self_reproduction).
affects_constraint(rec_02, io_dependency_exclusion).

veto_actor(computation_is_transformation_axiom).
veto_exposed(computation_is_transformation_axiom, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (self-creation) is 1.0, and Stability is absolute once the fixed point is found.
measurement(0, [1.00, 0.95, 0.30, 0.80]).
measurement(1, [1.00, 1.00, 0.40, 1.00]).

% 6. INTENT EVIDENCE
% Alternative: Simple echo command (Discarded: fails to be a self-contained program)
% Beneficiaries: Theoretical computer scientists and virus researchers (Malware self-prop)
% Power Delta: Representation vs Execution (Source text vs Processed output)
intent_evidence(recursion_theorem_application, high_delta).
