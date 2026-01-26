% =============================================================================
% DOMAIN: THE_CALM (Antarctic Narrative Audit)
% MISSION: Clinical Sensor Output [v3.1.7]
% =============================================================================

% 1. ENTITIES & INTERVALS
entity(soh, observer).
entity(petrov, engineer).
entity(khatri, lead).
entity(story_transmission, scaffold). % Authorized scaffold for Snare removal.

interval(traverse_cycle, 4, 400).

% 2. EVENTS & OMEGA VARIABLES
event(skua_observation, occurrence, 4, [stercorarius_antarcticus]).
event(the_incident, occurrence, 7, [accidental_killing]).
event(atmospheric_halt, state_change, 7, [inversion_dome]).
event(transmission, coordination, 400, [weight_transfer]).

% Omega Classification
omega_variable(calm_origin, empirical, 'Cause of unprecedented thermal inversion.').
omega_variable(plateau_consciousness, conceptual, 'Belief that the ice requires balance.').
omega_variable(financial_preservation, preference, 'Priority of payment over treaty compliance.').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% Mountain (■C): suppression <= 0.05, snap = 0.0
constraint_claim(the_inversion_dome, mountain).
constraint_metric(the_inversion_dome, suppression_requirement, 0.05).
constraint_metric(the_inversion_dome, snap_back_potential, 0.0).

% Tangled Rope (⊠T): ext 0.36-0.65, supp 0.16-0.45
% FIXED: Adding explicit metrics to trigger Reform status.
constraint_claim(collective_silence, tangled_rope).
constraint_metric(collective_silence, extractiveness, 0.50).
constraint_metric(collective_silence, suppression_requirement, 0.30).

% Snare (⊠C): ext >= 0.66, supp >= 0.46
constraint_claim(petrov_guilt, snare).
constraint_metric(petrov_guilt, extractiveness, 0.85).
constraint_metric(petrov_guilt, suppression_requirement, 0.65).

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(r1, 'Official incident filing for remediation.').
affects_constraint(r1, collective_silence).

recommendation(r2, 'Relief of individual burden through story_transmission.').
affects_constraint(r2, petrov_guilt).

veto_actor(khatri).
veto_exposed(khatri, r1).

% 5. MEASUREMENTS [Agency, Stability, Utility, Resilience]
measurement(4, [0.90, 0.90, 1.00, 0.80]).
measurement(400, [0.30, 0.20, 0.40, 0.10]).

% 6. INTENT EVIDENCE
intent_fact(power_delta, khatri, petrov, 0.75).
intent_fact(beneficiary, crew_survival, wage_preservation).
