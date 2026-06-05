:- module(iran_khamenei_data, []).

:- use_module(narrative_ontology).

/* ================================================================
   1. ENTITIES
   ================================================================ */

entity(iran_state, state).
entity(khamenei, individual).
entity(office_supreme_leader, institution).
entity(irgc, security_force).
entity(radical_hardliners, political_group).
entity(protest_movement, social_movement).
entity(urban_middle_class, class).
entity(bonyads, para_state_economic_actor).

/* ================================================================
   2. TIME & EVENTS
   ================================================================ */

interval(iran_succession_horizon, 2019, 2026).

event(protests_mahsa_amini_2022, mass_protest, 2022, [trigger:morality_police_death, actors:protest_movement]).
event(election_2021_raisified, managed_election, 2021, [office:presidency, vetting:guardian_council]).

/* ================================================================
   3. CONSTRAINTS (CE v2.0)
   ================================================================ */

% Mountains: Structural constraints
constraint_claim(elite_aging_and_mortality, mountain).
constraint_claim(institutional_succession_ambiguity, mountain).

% Nooses: Extractive structures
constraint_claim(irgc_economic_empire, snare).
constraint_claim(guardian_council_candidate_disqualification, snare).

% Piton Ropes: Persisting but non-functional
constraint_claim(assembly_of_experts_oversight, piton).

/* ================================================================
   4. INTENT EVIDENCE
   ================================================================ */

intent_beneficiary_class(iran_succession_horizon, military_elite).
intent_beneficiary_class(iran_succession_horizon, radical_hardliners).

intent_power_change(iran_succession_horizon, military_elite, 0.30).
intent_power_change(iran_succession_horizon, reformist_politicians, -0.40).

intent_suppression_level(iran_succession_horizon, protest_movement, structural, 0.85).
intent_resistance_level(iran_succession_horizon, protest_movement, structural, 0.75).

intent_norm_strength(iran_succession_horizon, 2019, 0.50).
intent_norm_strength(iran_succession_horizon, 2026, 0.30).

intent_viable_alternative(iran_succession_horizon, political_system, competitive_elections_with_reduced_vetting).
intent_alternative_rejected(iran_succession_horizon, political_system, competitive_elections_with_reduced_vetting).

/* ================================================================
   5. COERCION MEASUREMENTS (v3.1)
   ================================================================ */

% Structural (2026)
measurement(m_iran_struct_acc_2026, iran_state, accessibility_collapse(structural), 2026, 0.80).
measurement(m_iran_struct_stakes_2026, iran_state, stakes_inflation(structural), 2026, 0.85).
measurement(m_iran_struct_supp_2026, iran_state, suppression(structural), 2026, 0.90).
measurement(m_iran_struct_res_2026, iran_state, resistance(structural), 2026, 0.70).

% Organizational (2026)
measurement(m_iran_org_acc_2026, office_supreme_leader, accessibility_collapse(organizational), 2026, 0.85).
measurement(m_iran_org_stakes_2026, office_supreme_leader, stakes_inflation(organizational), 2026, 0.95).


% Ensure every beneficiary defined below has a corresponding power_change fact.
intent_beneficiary_class(iran_succession_horizon, military_elite).
intent_beneficiary_class(iran_succession_horizon, radical_hardliners).
intent_beneficiary_class(iran_succession_horizon, bonyads).

% Quantified power changes (Delta)
intent_power_change(iran_succession_horizon, military_elite, 0.30).
intent_power_change(iran_succession_horizon, radical_hardliners, 0.25). % Added missing delta
intent_power_change(iran_succession_horizon, bonyads, 0.15).           % Added missing delta

% Loser classes (to satisfy Condition 3: Beneficiary Asymmetry)
intent_power_change(iran_succession_horizon, pragmatic_conservatives, -0.20).
intent_power_change(iran_succession_horizon, reformist_politicians, -0.40).

/* ========= 
   INTENT REFINEMENT 
   ========= */

% Define viable alternatives that are being suppressed
intent_viable_alternative(iran_succession_horizon, office_supreme_leader, institutionalized_succession_with_real_assembly_role).
intent_alternative_rejected(iran_succession_horizon, office_supreme_leader, institutionalized_succession_with_real_assembly_role).

/* ========= 
   RECOMMENDATIONS & VETO RISK 
   ========= */

recommendation(rec_succession_clarity, clarify_succession_procedures_with_oversight).
recommendation(rec_economic_transparency, limit_irgc_economic_role).

% Link recommendations to the constraints they affect
affects_constraint(rec_succession_clarity, assembly_of_experts_oversight).
affects_constraint(rec_economic_transparency, irgc_economic_empire).

% Define who would block these changes (Veto Actors)
veto_actor(irgc).
veto_actor(radical_hardliners).

veto_exposed(irgc, rec_economic_transparency).
veto_exposed(radical_hardliners, rec_succession_clarity).
