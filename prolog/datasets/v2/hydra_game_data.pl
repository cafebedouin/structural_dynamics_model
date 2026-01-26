% DOMAIN: Hydra Game [v3.1.2 DR Audit Edition]

% 1. ENTITIES & INTERVALS
entity(hydra_tree_structure, system).
entity(epsilon_zero_scaffold, scaffold).

% Integers used for calculation stability per Audit Standards.
interval(game_duration, 1, 1000).

% 2. EVENTS & OMEGA VARIABLES
event(ev_01, head_removal, 1, [target(leaf_node), result(replication)]).

% Omega Variable (Conceptual): The ambiguity of applying finite induction to a process requiring transfinite ordinals.
omega_variable(om_01, conceptual, arithmetical_unprovability_of_termination).

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% Mountain Verification (■C)
constraint_claim(tree_topology, mountain).
constraint_metric(tree_topology, suppression_requirement, 0.05).
constraint_metric(tree_topology, snap_back_potential, 0.0).

% Tangled Rope Verification (⊠T)
% Current extractiveness (0.75) exceeds Tangled Rope limit (0.65), triggering Snare logic.
constraint_claim(subtree_proliferation, snare).
constraint_metric(subtree_proliferation, extractiveness, 0.75).
constraint_metric(subtree_proliferation, suppression_requirement, 0.46).

% Snare Verification (⊠C)
% Requires extractiveness >= 0.66 AND suppression_requirement >= 0.46
constraint_claim(arithmetical_provability_limit, snare).
constraint_metric(arithmetical_provability_limit, extractiveness, 0.99).
constraint_metric(arithmetical_provability_limit, suppression_requirement, 0.46).

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(rec_01, cut_noose_via_epsilon_zero_scaffold).
affects_constraint(rec_01, arithmetical_provability_limit).

recommendation(rec_02, adopt_transfinite_induction).
affects_constraint(rec_02, subtree_proliferation).

veto_actor(peano_arithmetic).
veto_exposed(peano_arithmetic, rec_02).

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
% Derived from base measurements m1-m4 (Start: 1) and m5-m8 (End: 1000)
measurement(1, [1.0, 1.0, 0.80, 0.95]).
measurement(1000, [1.0, 0.0, 0.99, 0.05]).

% 6. INTENT EVIDENCE
intent_fact(viable_alternative, game_duration, standard_induction, 0.05).
intent_fact(beneficiary, proof_theorists, logical_consistency, 1.0).
intent_fact(power_change, replication_count, ordinal_reduction, 0.95).
