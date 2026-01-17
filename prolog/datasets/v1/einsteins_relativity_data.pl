% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Einstein's Relativity
% Domain: Physics - Transition from Classical Mechanics to General Relativity
% ==========================================================

% --- 1. Entities & Intervals ---
entity(albert_einstein, individual).
entity(isaac_newton, individual).
entity(h_a_lorentz, individual).
entity(physics_community, class).
entity(theoretical_physics_framework, organizational).
entity(spacetime_continuum, structural).

interval(relativity_transition, 0, 100). % From Classical (T0) to General Relativity (Tn) 

% --- 2. Events ---
event(ev01_special_theory_pub, theoretical_initiation, 5, [actor(albert_einstein), concept(constancy_of_c)]). % 
event(ev02_fizeau_experiment, empirical_validation, 45, [subject(velocity_addition), result(lorentz_transform_match)]). % 
event(ev03_general_theory_pub, systemic_overhaul, 85, [actor(albert_einstein), concept(equivalence_principle)]). % 
event(ev04_solar_eclipse_expedition, final_verification, 95, [actor(eddington), subject(light_deflection), result(confirmed)]). % 

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute constancy of the velocity of light in vacuo (c). 
constraint_claim(velocity_of_light_constancy, mountain).
constraint_metric(velocity_of_light_constancy, accessibility_collapse, 1.0).

% Noose: The 'Promise' of Classical Mechanics' Absolute Time. It tightens as velocities approach c. 
constraint_claim(absolute_time_notion, noose).
constraint_metric(absolute_time_notion, stakes_inflation, 0.95).

% Zombie: The Aether. A theoretical medium formerly believed to be at rest but rendered unnecessary. 
constraint_claim(luminiferous_aether, zombie).
constraint_metric(luminiferous_aether, suppression, 0.85).

% Rope: The Galilei Transformation. Ties local events to a specific unique reference body. 
constraint_claim(galilei_transformation, rope).
constraint_metric(galilei_transformation, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Replace absolute time with the relativity of simultaneity to resolve electromagnetic conflicts.'). % 
recommendation(rec02, 'Incorporate non-Euclidean Gaussian coordinates to describe the gravitational field accurately.'). % 

affects_constraint(rec01, absolute_time_notion).
affects_constraint(rec02, spacetime_continuum).

veto_actor(isaac_newton).
veto_actor(physics_community).

veto_exposed(isaac_newton, rec01).
veto_exposed(physics_community, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Classical Mechanics Dominance)
measurement(m01, albert_einstein, accessibility_collapse(individual), 0, 0.80).
measurement(m02, albert_einstein, stakes_inflation(individual), 0, 0.50).
measurement(m03, albert_einstein, suppression(individual), 0, 0.90).
measurement(m04, albert_einstein, resistance(individual), 0, 0.10).

measurement(m05, theoretical_physics_framework, accessibility_collapse(organizational), 0, 0.10).
measurement(m06, theoretical_physics_framework, stakes_inflation(organizational), 0, 0.20).
measurement(m07, theoretical_physics_framework, suppression(organizational), 0, 0.05).
measurement(m08, theoretical_physics_framework, resistance(organizational), 0, 0.05).

measurement(m09, physics_community, accessibility_collapse(class), 0, 0.05).
measurement(m10, physics_community, stakes_inflation(class), 0, 0.10).
measurement(m11, physics_community, suppression(class), 0, 0.05).
measurement(m12, physics_community, resistance(class), 0, 0.95).

measurement(m13, spacetime_continuum, accessibility_collapse(structural), 0, 0.00). % 
measurement(m14, spacetime_continuum, stakes_inflation(structural), 0, 0.05).
measurement(m15, spacetime_continuum, suppression(structural), 0, 0.00). % Beneficiary logic
measurement(m16, spacetime_continuum, resistance(structural), 0, 0.00). % Beneficiary logic

% Time Tn (Relativistic Shift Finalized)
measurement(m17, albert_einstein, accessibility_collapse(individual), 100, 0.05).
measurement(m18, albert_einstein, stakes_inflation(individual), 100, 0.95).
measurement(m19, albert_einstein, suppression(individual), 100, 0.05).
measurement(m20, albert_einstein, resistance(individual), 100, 0.90).

measurement(m21, theoretical_physics_framework, accessibility_collapse(organizational), 100, 0.05).
measurement(m22, theoretical_physics_framework, stakes_inflation(organizational), 100, 0.90).
measurement(m23, theoretical_physics_framework, suppression(organizational), 100, 0.05).
measurement(m24, theoretical_physics_framework, resistance(organizational), 100, 0.95).

measurement(m25, physics_community, accessibility_collapse(class), 100, 0.05).
measurement(m26, physics_community, stakes_inflation(class), 100, 0.95).
measurement(m27, physics_community, suppression(class), 100, 0.20).
measurement(m28, physics_community, resistance(class), 100, 0.85).

measurement(m29, spacetime_continuum, accessibility_collapse(structural), 100, 0.00).
measurement(m30, spacetime_continuum, stakes_inflation(structural), 100, 0.30).
measurement(m31, spacetime_continuum, suppression(structural), 100, 0.00). % Beneficiary logic
measurement(m32, spacetime_continuum, resistance(structural), 100, 0.00). % Beneficiary logic

% --- 6. Intent Evidence ---
intent_viable_alternative(relativity_transition, spacetime_continuum, 'Extended_Euclidean_Aether_Models'). % 
intent_alternative_rejected(relativity_transition, spacetime_continuum, 'Extended_Euclidean_Aether_Models'). % 

intent_beneficiary_class(relativity_transition, theoretical_physics_framework).
intent_power_change(relativity_transition, theoretical_physics_framework, 0.90). % Structural gain from unified theory 

intent_loser_class(relativity_transition, physics_community). % Forced to unlearn classical intuition
intent_power_change(relativity_transition, physics_community, -0.40). % High cognitive and structural overhaul cost 

intent_suppression_level(relativity_transition, theoretical_physics_framework, structural, 0.0).
intent_resistance_level(relativity_transition, theoretical_physics_framework, structural, 0.0).

intent_norm_strength(relativity_transition, 0, 0.95). % Newton's Laws as dogma 
intent_norm_strength(relativity_transition, 100, 0.98). % Einstein's Laws as new universal standard 
