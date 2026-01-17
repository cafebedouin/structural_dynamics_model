% Clinical Sensor Output: Stable Marriage Problem (SMP) - Refined Analysis
% Audit Status: Resolved Logic Error in Alternative Rejection

% --- Section 1: Entities & Intervals ---
entity(men_class, class).
entity(women_class, class).
entity(gs_protocol, structural). 
entity(egalitarian_scaffold, scaffold). 

interval(market_evolution, 0, 100).

% --- Section 2: Events ---
event(e1, protocol_deployment, 0, [type(men_proposing)]).
event(e2, preference_rigidity, 50, [type(agent_polarization)]).

% --- Section 3: Constraint Claims & Metrics ---
% The GS Protocol falsely claims 'Mountain' status (natural necessity)
constraint_claim(gs_protocol, mountain).

% Current state metrics (at T=100)
% High X and E correctly trigger the "False Mountain" (Type I Error) audit alert
constraint_metric(gs_protocol, extractiveness, 0.75).
constraint_metric(gs_protocol, suppression_requirement, 0.70).
constraint_metric(gs_protocol, snap_back_potential, 0.45).

% --- Section 4: Temporal Measurements (Modal Logic Data) ---
% T=0: Initial state - Functional 'Rope'
measurement(m1, gs_protocol, extractiveness, 0, 0.25).
measurement(m2, gs_protocol, suppression_requirement, 0, 0.20).

% T=50: Middle state - Transitioning to 'Tangled Rope'
measurement(m3, gs_protocol, extractiveness, 50, 0.45).
measurement(m4, gs_protocol, suppression_requirement, 50, 0.40).

% T=100: Final state - Full 'Noose' capture
measurement(m5, gs_protocol, extractiveness, 100, 0.75).
measurement(m6, gs_protocol, suppression_requirement, 100, 0.70).

% --- Section 5: Dependencies ---
affects_constraint(gs_protocol, women_class).
affects_constraint(gs_protocol, men_class).

% --- Section 6: Intent Evidence (FIXED LOGIC) ---
% The alternative must match exactly for the audit to verify the rejection logic
intent_beneficiary_class(market_evolution, men_class).
intent_power_change(market_evolution, men_class, 0.65).

% Defining the viable alternative
intent_viable_alternative(market_evolution, women_class, 'egalitarian_mcvitie_wilson_algorithm').

% Referencing the same alternative as the one rejected by the beneficiary class
intent_alternative_rejected(market_evolution, men_class, 'egalitarian_mcvitie_wilson_algorithm').

% --- Section 7: Recommendations ---
recommendation(rec1, 'Transition to Egalitarian Scaffold to mitigate proposer-optimal capture').
affects_constraint(rec1, gs_protocol).

% --- Section 8: Omega Variables ---
omega_variable(omega1, conceptual, 'Degree of agent satisfaction beyond rank-based metrics').
