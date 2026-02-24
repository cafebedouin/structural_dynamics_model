% DOMAIN: P vs NP [v3.1.2 DR Audit Edition]

% 1. ENTITIES & INTERVALS
entity(complexity_class_p, system).
entity(cook_levin_scaffold, scaffold).

% Standardized integer intervals for v3.1.2 Audit compliance.
interval(computation_time, 0, 100).
interval(verification_time, 0, 100).

% 2. EVENTS & OMEGA VARIABLES
event(ev_01, reduction_execution, 50, [from(np_complete), to(sat)]).

% Omega Variable (Conceptual): The inherent limitation of current proof techniques against relativizing or natural proofs.
omega_variable(om_01, conceptual, limitation_of_diagonalization_techniques).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% Mountain Verification (■C)
constraint_claim(deterministic_tractability, mountain).
constraint_metric(deterministic_tractability, suppression_requirement, 0.05).
constraint_metric(deterministic_tractability, snap_back_potential, 0.0).

% Tangled Rope Verification (⊠T)
% Complexity of SAT reductions and completeness linkage.
constraint_claim(completeness_structural_linkage, tangled_rope).
constraint_metric(completeness_structural_linkage, extractiveness, 0.50).
constraint_metric(completeness_structural_linkage, intensity, 0.35).

% Snare Verification (⊠C)
% Requires extractiveness >= 0.66 AND suppression_requirement >= 0.46
constraint_claim(relativization_and_algebrization_barrier, snare).
constraint_metric(relativization_and_algebrization_barrier, extractiveness, 0.99).
constraint_metric(relativization_and_algebrization_barrier, suppression_requirement, 0.75).

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(rec_01, reform_structural_linkage_via_cook_levin_scaffold).
affects_constraint(rec_01, completeness_structural_linkage).

recommendation(rec_02, abandon_standard_diagonalization).
affects_constraint(rec_02, relativization_and_algebrization_barrier).

veto_actor(oracle_results).
veto_exposed(oracle_results, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Derived from base measurements m1-m4 (Start: 0) and m5-m8 (End: 100)
measurement(0, [1.0, 1.0, 0.90, 0.95]).
measurement(100, [1.0, 0.0, 1.0, 0.10]).

% 6. INTENT EVIDENCE
intent_fact(viable_alternative, computation_time, p_equals_pspace, 0.30).
intent_fact(beneficiary, cryptographers, security_utility, 1.0).
intent_fact(power_change, search_complexity, verification_efficiency, 0.90).
