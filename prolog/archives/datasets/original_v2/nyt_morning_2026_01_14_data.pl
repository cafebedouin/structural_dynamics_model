% --- 1. Entities & Intervals ---
entity(minneapolis_urban_system, process).
entity(federal_enforcement_pool, group).
entity(resident_activist_pool, group).
entity(somali_diaspora, group).
entity(legal_injunction_scaffold, scaffold).
entity(police_reform_scaffold, scaffold).

interval(crisis_timeline, 0, 10).

% --- 2. Events & Omega Variables ---
event(renee_good_shooting, initialization, 0, [actor(federal_agent), victim(renee_good)]).
event(immigration_surge, expansion, 3, [count(3000_agents), action(neighborhood_patrols)]).
event(state_legal_filing, resistance, 5, [actor(minnesota_st_paul_minneapolis), action(lawsuit)]).
event(civil_unrest_viral, observation, 7, [state(viral_videos), condition(military_occupation_feel)]).
event(prosecutor_resignations, conflict, 9, [count(6), cause(justice_department_bias)]).

omega_variable(omega_1, conceptual, definition_of_disrespect_as_protected_speech_vs_interference).
omega_variable(omega_2, preference, prioritizing_federal_sovereignty_over_municipal_trust).
omega_variable(omega_3, empirical, probability_of_2020_scale_destruction_given_current_tensions).

% --- 4. Constraint Claims & Kinetic Metrics ---

% MOUNTAIN: The First Amendment / Protected Speech
constraint_claim(constitutional_speech_protection, mountain).
constraint_metric(constitutional_speech_protection, intensity, 0.05).
constraint_metric(constitutional_speech_protection, suppression_requirement, 0.05).
constraint_metric(constitutional_speech_protection, snap_back_potential, 0.0).
constraint_metric(constitutional_speech_protection, extractiveness, 0.02).

% ROPE: Standard Law Enforcement Boundaries
constraint_claim(ordinary_law_enforcement_norms, rope).
constraint_metric(ordinary_law_enforcement_norms, intensity, 0.35).
constraint_metric(ordinary_law_enforcement_norms, suppression_requirement, 0.15).
constraint_metric(ordinary_law_enforcement_norms, snap_back_potential, 0.10).
constraint_metric(ordinary_law_enforcement_norms, extractiveness, 0.35).

% TANGLED ROPE: Local-Federal Jurisdictional Conflict
constraint_claim(jurisdictional_cooperation_friction, tangled_rope).
constraint_metric(jurisdictional_cooperation_friction, intensity, 0.60).
constraint_metric(jurisdictional_cooperation_friction, suppression_requirement, 0.40).
constraint_metric(jurisdictional_cooperation_friction, snap_back_potential, 0.50).
constraint_metric(jurisdictional_cooperation_friction, extractiveness, 0.55).

% NOOSE: Escalating Military-Style Occupation
constraint_claim(military_occupation_escalation, snare).
constraint_metric(military_occupation_escalation, intensity, 0.95).
constraint_metric(military_occupation_escalation, suppression_requirement, 0.85).
constraint_metric(military_occupation_escalation, snap_back_potential, 0.90).
constraint_metric(military_occupation_escalation, extractiveness, 0.88).

% --- 4. Recommendations & Veto Points ---
recommendation(rec_01, accept_constitutional_limits_on_retribution_rhetoric).
affects_constraint(rec_01, constitutional_speech_protection).

recommendation(rec_02, maintain_police_chief_ohara_trust_building_measures).
affects_constraint(rec_02, ordinary_law_enforcement_norms).

recommendation(rec_03, reform_federal_deployment_via_judicial_injunction).
affects_constraint(rec_03, jurisdictional_cooperation_friction).

recommendation(rec_04, cut_federal_surge_to_prevent_city_destruction).
affects_constraint(rec_04, military_occupation_escalation).

veto_actor(trump_administration).
veto_exposed(trump_administration, rec_04).
veto_actor(federal_prosecutors_resigned).
veto_exposed(federal_prosecutors_resigned, rec_03).

% --- 5. Measurements ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.70, 0.40, 0.50, 0.45]).
measurement(10, [0.15, 0.05, 0.10, 0.02]).

% --- 6. Intent Evidence ---
intent_alternative(de_escalation_protocol, bypassed_by_administration).
intent_beneficiary(executive_political_base, rhetoric_of_reckoning).
intent_power_delta(federal_ice_vs_municipal_government, high_asymmetric_force).
