% 1. ENTITIES & INTERVALS
entity(ordinal_collection, system).
entity(well_ordered_manifold, manifold).
entity(succession_path, sensor_path).
entity(von_neumann_universe, scaffold).

interval(ordinal_progression, 0, 1000).
interval(contradiction_point, 0, 1).

% 2. EVENTS & OMEGA VARIABLES
event(e1, ordinal_assembly, 0, [type, total_ordering]).
event(e2, totality_clash, 1, [ordinal_of_ON, omega_plus_one]).

% Omega Variable: Conceptual (Defining the boundary between 'sets' and 'proper classes')
omega_variable(omega_c1, conceptual, validity_of_unrestricted_comprehension_for_well_ordered_types).

% Omega Variable: Empirical (Resolution limits of representing nested transfinite structures)
omega_variable(omega_e1, empirical, symbolic_truncation_of_infinite_ordinal_notations).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% The Well-Ordering Invariant (The Mountain)
% Every set of ordinals is well-ordered; this internal property of ordinals is immutable.
constraint_claim(internal_well_ordering, mountain).
constraint_metric(internal_well_ordering, intensity, 1.0).
constraint_metric(internal_well_ordering, suppression_requirement, 0.05).
constraint_metric(internal_well_ordering, snap_back_potential, 0.0).
constraint_metric(internal_well_ordering, extractiveness, 0.01).

% The Successor Expansion (The Tangled Rope)
% For any set of ordinals, there is always a larger ordinal (the successor of the set's sup).
% Extractiveness at 0.55 triggers a Reform recommendation for class-level segregation.
constraint_claim(successor_mapping_complexity, tangled_rope).
constraint_metric(successor_mapping_complexity, intensity, 0.88).
constraint_metric(successor_mapping_complexity, suppression_requirement, 0.42).
constraint_metric(successor_mapping_complexity, snap_back_potential, 0.75).
constraint_metric(successor_mapping_complexity, extractiveness, 0.55).

% The Totality Sink (The Snare)
% The assumption that the "set of all ordinals" exists. This "strangles" the hierarchy.
% Extractiveness at 0.99 requires the 'von_neumann_universe' scaffold to resolve as a proper class.
constraint_claim(ordinal_totality_comprehension, snare).
constraint_metric(ordinal_totality_comprehension, intensity, 0.99).
constraint_metric(ordinal_totality_comprehension, suppression_requirement, 0.98).
constraint_metric(ordinal_totality_comprehension, snap_back_potential, 0.01).
constraint_metric(ordinal_totality_comprehension, extractiveness, 0.99).

% 4. RECOMMENDATIONS & VETO POINTS
% Tangled Rope (0.55 extractiveness) triggers Reform
recommendation(rec_01, implement_type_theory_to_bound_ordinal_levels).
affects_constraint(rec_01, successor_mapping_complexity).

% Snare (0.99 extractiveness) triggers Cut
% Utilizing 'von_neumann_universe' scaffold to define Ω as a proper class, not a set.
recommendation(rec_02, abandon_naive_set_membership_for_class_abstraction).
affects_constraint(rec_02, ordinal_totality_comprehension).

veto_actor(cantor_diagonal_limit).
veto_exposed(cantor_diagonal_limit, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Agency (ordinal creation) is high, but Stability collapses if the set is closed.
measurement(0, [1.00, 1.00, 0.90, 0.95]).
measurement(1, [1.00, 0.00, 0.10, 0.05]).

% 6. INTENT EVIDENCE
% Alternative: Russell's Paradox (Discarded: Burali-Forti is specific to the structure of ordinals)
% Beneficiaries: Set theorists (ZF foundations) and Logicians (avoiding totality traps)
% Power Delta: Ordinal Rank (Element-level comparison vs Universal collection)
intent_evidence(proper_class_distinction, high_delta).
