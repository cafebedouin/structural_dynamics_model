% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: DNA Structure Discovery
% Domain: Molecular Biology - Discovery of the Double Helix (1953)
% ==========================================================

% --- 1. Entities & Intervals ---
entity(watson_crick, individual). 
entity(pauling_corey, individual). 
entity(kings_college, organizational). 
entity(cavendish_laboratory, organizational). 
entity(molecular_biology_community, class). 
entity(dna_chemical_system, structural). 

interval(discovery_validation_period, 0, 100). 

% --- 2. Events ---
event(ev01_pauling_model_review, model_rejection, 5, [actor(watson_crick), target(pauling_corey), reason(electrostatic_repulsion)]). 
event(ev02_double_helix_proposal, suggestion_of_structure, 10, [actor(watson_crick), structure(two_helical_chains)]). 
event(ev03_base_pairing_inference, specific_pairing_discovery, 45, [adenine_thymine(unity), guanine_cytosine(unity)]). 
event(ev04_copying_mechanism_note, implication_statement, 95, [subject(genetic_material_copying)]). 

% --- 3. Constraint Claims & Metrics ---
% Mountain: The physical-chemical laws of van der Waals distances and electrostatic repulsion.
constraint_claim(chemical_stereochemistry_limits, mountain). 
constraint_metric(chemical_stereochemistry_limits, accessibility_collapse, 0.95). 

% Noose: The specific base-pairing requirements (A-T, G-C). A tautomeric 'trap' that forces a specific sequence.
constraint_claim(specific_base_pairing_requirement, noose). 
constraint_metric(specific_base_pairing_requirement, stakes_inflation, 0.90). 

% Zombie: The three-chain structure model. A proposed model (Pauling/Fraser) that is functionally 'dead' due to instability but exists in the literature.
constraint_claim(three_chain_model, zombie). 
constraint_metric(three_chain_model, suppression, 0.75). 

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Adopt the keto tautomeric form assumption to resolve base-pairing specificities.'). 
recommendation(rec02, 'Utilize high-water content X-ray diffraction to confirm phosphate positioning on the exterior.'). 

affects_constraint(rec01, specific_base_pairing_requirement). 
affects_constraint(rec02, chemical_stereochemistry_limits). 

veto_actor(pauling_corey). 
veto_actor(molecular_biology_community). 

veto_exposed(pauling_corey, rec01). 
veto_exposed(molecular_biology_community, rec02). 

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (Initial Proposals / Competitive Models)
measurement(m01, watson_crick, accessibility_collapse(individual), 0, 0.40). 
measurement(m02, watson_crick, stakes_inflation(individual), 0, 0.60). 
measurement(m03, watson_crick, suppression(individual), 0, 0.30). 
measurement(m04, watson_crick, resistance(individual), 0, 0.50). 

measurement(m05, cavendish_laboratory, accessibility_collapse(organizational), 0, 0.20). 
measurement(m06, cavendish_laboratory, stakes_inflation(organizational), 0, 0.30). 
measurement(m07, cavendish_laboratory, suppression(organizational), 0, 0.10). 
measurement(m08, cavendish_laboratory, resistance(organizational), 0, 0.10). 

measurement(m09, molecular_biology_community, accessibility_collapse(class), 0, 0.50). 
measurement(m10, molecular_biology_community, stakes_inflation(class), 0, 0.70). 
measurement(m11, molecular_biology_community, suppression(class), 0, 0.80). 
measurement(m12, molecular_biology_community, resistance(class), 0, 0.20). 

measurement(m13, dna_chemical_system, accessibility_collapse(structural), 0, 0.00). 
measurement(m14, dna_chemical_system, stakes_inflation(structural), 0, 0.10). 
measurement(m15, dna_chemical_system, suppression(structural), 0, 0.00). 
measurement(m16, dna_chemical_system, resistance(structural), 0, 0.00). 

% Time Tn (Structure Publication / Mechanistic Inference)
measurement(m17, watson_crick, accessibility_collapse(individual), 100, 0.10). 
measurement(m18, watson_crick, stakes_inflation(individual), 100, 0.95). 
measurement(m19, watson_crick, suppression(individual), 100, 0.10). 
measurement(m20, watson_crick, resistance(individual), 100, 0.90). 

measurement(m21, cavendish_laboratory, accessibility_collapse(organizational), 100, 0.05). 
measurement(m22, cavendish_laboratory, stakes_inflation(organizational), 100, 0.80). 
measurement(m23, cavendish_laboratory, suppression(organizational), 100, 0.05). 
measurement(m24, cavendish_laboratory, resistance(organizational), 100, 0.95). 

measurement(m25, molecular_biology_community, accessibility_collapse(class), 100, 0.10). 
measurement(m26, molecular_biology_community, stakes_inflation(class), 100, 0.95). 
measurement(m27, molecular_biology_community, suppression(class), 100, 0.30). 
measurement(m28, molecular_biology_community, resistance(class), 100, 0.85). 

measurement(m29, dna_chemical_system, accessibility_collapse(structural), 100, 0.00). 
measurement(m30, dna_chemical_system, stakes_inflation(structural), 100, 0.20). 
measurement(m31, dna_chemical_system, suppression(structural), 100, 0.00). 
measurement(m32, dna_chemical_system, resistance(structural), 100, 0.00). 

% --- 6. Intent Evidence ---
intent_viable_alternative(discovery_validation_period, dna_chemical_system, 'Three_Chain_Phosphate_Axis_Model'). 
intent_alternative_rejected(discovery_validation_period, dna_chemical_system, 'Three_Chain_Phosphate_Axis_Model'). 

intent_beneficiary_class(discovery_validation_period, molecular_biology_community). 
intent_power_change(discovery_validation_period, molecular_biology_community, 0.80). 

intent_loser_class(discovery_validation_period, pauling_corey). 
intent_power_change(discovery_validation_period, pauling_corey, -0.60). 

intent_suppression_level(discovery_validation_period, molecular_biology_community, structural, 0.0). 
intent_resistance_level(discovery_validation_period, molecular_biology_community, structural, 0.0). 

intent_norm_strength(discovery_validation_period, 0, 0.40). 
intent_norm_strength(discovery_validation_period, 100, 0.95). 
