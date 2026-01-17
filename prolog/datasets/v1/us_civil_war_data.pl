% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: U.S. Civil War
% Domain: Political-Military History - The American Civil War (1861-1865)
% ==========================================================

% --- 1. Entities & Intervals ---
entity(abraham_lincoln, individual).
entity(us_federal_government, organizational).
entity(confederate_states_of_america, organizational).
entity(southern_planter_class, class).
entity(enslaved_population, class).
entity(federal_governance_structure, structural).

interval(us_civil_war, 1861, 1865).

% --- 2. Events ---
event(ev01_fort_sumter, kinetic_initiation, 1861, [actor(confederate_states_of_america), target(fort_sumter), result(declaration_of_war)]).
event(ev02_antietam_battle, state_stabilization, 1862, [actor(us_federal_government), effect(diplomatic_containment)]).
event(ev03_emancipation_proc, systemic_overhaul, 1863, [actor(abraham_lincoln), target(enslaved_population), property(legal_transformation)]).
event(ev04_appomattox_surrender, final_verification, 1865, [actor(robert_e_lee), target(ulysses_s_grant), result(structural_reintegration)]).

% --- 3. Constraint Claims & Metrics ---
% Mountain: Northern Industrial-Logistical Advantage. The absolute physical/economic output of the North.
constraint_claim(industrial_output_advantage, mountain).
constraint_metric(industrial_output_advantage, accessibility_collapse, 0.95).

% Noose: The Attrition Spiral. Confederate manpower limits and blockade constraints narrowing survival options.
constraint_claim(manpower_attrition_noose, noose).
constraint_metric(manpower_attrition_noose, stakes_inflation, 1.0).

% Zombie: Nullification Doctrine. The dead concept of state sovereignty used to justify secession while draining central C2 capability.
constraint_claim(nullification_doctrine, zombie).
constraint_metric(nullification_doctrine, suppression, 0.85).

% Rope: Habeas Corpus Suspension. Ties individual legal agency to executive military necessity.
constraint_claim(martial_law_tether, rope).
constraint_metric(martial_law_tether, suppression, 0.75).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt the Crittenden Compromise to permanently constitutionalize slavery and avoid kinetic escalation.').
recommendation(rec02, 'Implement the Anaconda Plan to strangle Southern commerce through naval isolation.').

affects_constraint(rec01, nullification_doctrine).
affects_constraint(rec02, industrial_output_advantage).

veto_actor(abraham_lincoln).
veto_actor(southern_planter_class).

veto_exposed(abraham_lincoln, rec01). % Lincoln vetoed the extension of slavery.
veto_exposed(southern_planter_class, rec02). % Class exposed to total economic strangulation.

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (1861: Outbreak of War)
measurement(m01, abraham_lincoln, accessibility_collapse(individual), 1861, 0.40).
measurement(m02, abraham_lincoln, stakes_inflation(individual), 1861, 0.90).
measurement(m03, abraham_lincoln, suppression(individual), 1861, 0.20).
measurement(m04, abraham_lincoln, resistance(individual), 1861, 0.80).

measurement(m05, us_federal_government, accessibility_collapse(organizational), 1861, 0.30).
measurement(m06, us_federal_government, stakes_inflation(organizational), 1861, 0.70).
measurement(m07, us_federal_government, suppression(organizational), 1861, 0.10).
measurement(m08, us_federal_government, resistance(organizational), 1861, 0.05).

measurement(m09, southern_planter_class, accessibility_collapse(class), 1861, 0.10).
measurement(m10, southern_planter_class, stakes_inflation(class), 1861, 0.60).
measurement(m11, southern_planter_class, suppression(class), 1861, 0.05).
measurement(m12, southern_planter_class, resistance(class), 1861, 0.95).

measurement(m13, federal_governance_structure, accessibility_collapse(structural), 1861, 0.05).
measurement(m14, federal_governance_structure, stakes_inflation(structural), 1861, 0.15).
measurement(m15, federal_governance_structure, suppression(structural), 1861, 0.00). % Beneficiary Logic
measurement(m16, federal_governance_structure, resistance(structural), 1861, 0.00). % Beneficiary Logic

% Time Tn (1865: Collapse of Confederacy)
measurement(m17, abraham_lincoln, accessibility_collapse(individual), 1865, 0.05).
measurement(m18, abraham_lincoln, stakes_inflation(individual), 1865, 1.00).
measurement(m19, abraham_lincoln, suppression(individual), 1865, 0.10).
measurement(m20, abraham_lincoln, resistance(individual), 1865, 1.00).

measurement(m21, us_federal_government, accessibility_collapse(organizational), 1865, 0.00).
measurement(m22, us_federal_government, stakes_inflation(organizational), 1865, 0.95).
measurement(m23, us_federal_government, suppression(organizational), 1865, 0.05).
measurement(m24, us_federal_government, resistance(organizational), 1865, 1.00).

measurement(m25, southern_planter_class, accessibility_collapse(class), 1865, 1.00). % Total loss of agency
measurement(m26, southern_planter_class, stakes_inflation(class), 1865, 1.00). % Total property/political loss
measurement(m27, southern_planter_class, suppression(class), 1865, 0.95). % Reconstruction pressure
measurement(m28, southern_planter_class, resistance(class), 1865, 0.10). % Submission to federal mandate

measurement(m29, federal_governance_structure, accessibility_collapse(structural), 1865, 0.00).
measurement(m30, federal_governance_structure, stakes_inflation(structural), 1865, 0.60).
measurement(m31, federal_governance_structure, suppression(structural), 1865, 0.00). % Beneficiary Logic
measurement(m32, federal_governance_structure, resistance(structural), 1865, 0.00). % Beneficiary Logic

% --- 6. Intent Evidence ---
intent_viable_alternative(us_civil_war, federal_governance_structure, 'Crittenden_Compromise_Permanence').
intent_alternative_rejected(us_civil_war, federal_governance_structure, 'Crittenden_Compromise_Permanence').

intent_beneficiary_class(us_civil_war, us_federal_government).
intent_power_change(us_civil_war, us_federal_government, 0.90). 

intent_loser_class(us_civil_war, southern_planter_class).
intent_power_change(us_civil_war, southern_planter_class, -0.95). 

intent_suppression_level(us_civil_war, us_federal_government, structural, 0.0).
intent_resistance_level(us_civil_war, us_federal_government, structural, 0.0).

intent_norm_strength(us_civil_war, 1861, 0.40). 
intent_norm_strength(us_civil_war, 1865, 1.00).
