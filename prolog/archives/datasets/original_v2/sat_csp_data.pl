% 1. ENTITIES & INTERVALS
entity(boolean_formula_phi, system).
entity(assignment_space_2n, manifold).
entity(conflict_resolution_path, sensor_path).
entity(dpll_cdcl_scaffold, scaffold).

interval(variable_count, 1, 1000000).
interval(satisfiability_check, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, unit_propagation, 0, [reduction, linear]).
event(e2, conflict_clause_learning, 1, [backtrack_level, jump]).

% Omega Variable: Conceptual (The P vs NP boundary applied to restricted CSPs)
omega_variable(omega_c1, conceptual, dichotomy_theorem_for_csps_on_finite_domains).

% Omega Variable: Empirical (The efficiency of CDCL solvers on industrial SAT instances)
omega_variable(omega_e1, empirical, unexpected_tractability_of_real_world_structured_formulas).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Boolean Foundation (The Mountain)
% The fundamental laws of logic (AND, OR, NOT) are the rigid, invariant physics of the problem.
constraint_claim(logical_consistency, mountain).
constraint_metric(logical_consistency, intensity, 1.0).
constraint_metric(logical_consistency, suppression_requirement, 0.05).
constraint_metric(logical_consistency, snap_back_potential, 0.0).
constraint_metric(logical_consistency, extractiveness, 0.01).

% The Constraint Interference (The Tangled Rope)
% Overlapping clauses (e.g., (x or y) and (not x or z)) create local and global entanglements.
% Extractiveness at 0.65 triggers a Reform recommendation for clause-learning.
constraint_claim(clause_density_entanglement, tangled_rope).
constraint_metric(clause_density_entanglement, intensity, 0.88).
constraint_metric(clause_density_entanglement, suppression_requirement, 0.45).
constraint_metric(clause_density_entanglement, snap_back_potential, 0.75).
constraint_metric(clause_density_entanglement, extractiveness, 0.65).

% The Exponential Time Hypothesis (The Snare)
% The belief that 3-SAT requires $2^{\delta n}$ time; the absolute "Snare" for worst-case scaling.
% Extractiveness at 0.99 requires the 'dpll_cdcl_scaffold' to achieve practical performance.
constraint_claim(worst_case_exponential_bound, snare).
constraint_metric(worst_case_exponential_bound, intensity, 0.99).
constraint_metric(worst_case_exponential_bound, suppression_requirement, 0.96).
constraint_metric(worst_case_exponential_bound, snap_back_potential, 0.02).
constraint_metric(worst_case_exponential_bound, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.65 extractiveness) triggers Reform
recommendation(rec_01, utilize_conflict_driven_clause_learning_to_decouple_search_space).
affects_constraint(rec_01, clause_density_entanglement).

% Snare (0.99 extractiveness) triggers Cut
% Utilizing 'dpll_cdcl_scaffold' to focus on industrial-track heuristics rather than worst-case theory.
recommendation(rec_02, adopt_stochastic_local_search_for_under_constrained_instances).
affects_constraint(rec_02, worst_case_exponential_bound).

veto_actor(schaefer_dichotomy_limit).
veto_exposed(schaefer_dichotomy_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (solver engineering) is 1.0; Stability is high for 2-SAT but zero for 3-SAT.
measurement(0, [1.00, 1.00, 0.90, 0.95]).
measurement(1, [1.00, 0.10, 0.99, 0.30]).

% 6. INTENT EVIDENCE
% Alternative: 2-SAT (Discarded: tractable and lacks the complexity depth of NP-completeness)
% Beneficiaries: Hardware verifiers, AI planners, and software security auditors
% Power Delta: Search vs Pruning (Exhaustive assignment vs Contradiction-driven pruning)
intent_evidence(universal_reduction_target, high_delta).
