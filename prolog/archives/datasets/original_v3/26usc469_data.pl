% Clinical Sensor Output: 26 USC 469 (Passive Activity Loss Rules)
% v3.1.2 DR Modal Logic Edition

% --- SECTION 1: ENTITIES & INTERVALS ---
entity(tax_reform_act_1986, individual).
entity(section_469, structural).
entity(rental_real_estate_industry, class).
entity(oil_gas_working_interests, class).
entity(closely_held_c_corps, class).
entity(repro_special_rules_1993, scaffold). % Real Estate Professional rules as transition support 

% Time interval representing the evolution of the statute (1986 to 2000 context)
interval(statute_evolution, 1986, 2000).

% --- SECTION 2: EVENTS ---
event(e1, enactment, 1986, [source(pub_l_99_514)]). event(e2, repro_expansion, 1993, [source(pub_l_103_66)]). 

% --- SECTION 3: CONSTRAINT CLAIMS & METRICS ---
% The statute claims to be a necessary structural necessity (Mountain) to prevent tax shelters.
constraint_claim(section_469, mountain).

% Current state metrics (approximate end-state at T=2000)
% High extractiveness due to complex compliance requirements and disallowed losses. 
constraint_metric(section_469, extractiveness, 0.45).
% High suppression due to the material participation standard and rental loss limits. 
constraint_metric(section_469, suppression_requirement, 0.55).
constraint_metric(section_469, snap_back_potential, 0.0).

% --- SECTION 4: TEMPORAL MEASUREMENTS (MODAL LOGIC) ---

% T=1986: Initial Enactment - High initial suppression, but moderate extractiveness (Phase-in)
measurement(m1, section_469, extractiveness, 1986, 0.30).
measurement(m2, section_469, suppression_requirement, 1986, 0.60). 

% T=1991: End of Phase-in - Suppression and extractiveness increase as transitions expire
measurement(m3, section_469, extractiveness, 1991, 0.55).
measurement(m4, section_469, suppression_requirement, 1991, 0.70). 

% T=1993: Real Estate Pro (REPRO) Scaffold introduced - Softens suppression for specific class
measurement(m5, section_469, extractiveness, 1993, 0.45).
measurement(m6, section_469, suppression_requirement, 1993, 0.50). 

% T=2000: Calcification - System remains a "Tangled Rope" but with "Snare" characteristics for small investors
measurement(m7, section_469, extractiveness, 2000, 0.45).
measurement(m8, section_469, suppression_requirement, 2000, 0.55).

% --- SECTION 5: DEPENDENCIES ---
% Broad economic impacts: 469 affects investment behavior across multiple sectors.
affects_constraint(section_469, rental_real_estate_industry). affects_constraint(section_469, oil_gas_working_interests). affects_constraint(section_469, closely_held_c_corps). 

% --- SECTION 6: INTENT EVIDENCE ---
% Oil and gas working interests received specific exemption from passive loss rules. 
intent_beneficiary_class(statute_evolution, oil_gas_working_interests).
intent_power_change(statute_evolution, oil_gas_working_interests, 0.40).

% Real estate professionals received significant relief in 1993. 
intent_beneficiary_class(statute_evolution, rental_real_estate_industry).
intent_power_change(statute_evolution, rental_real_estate_industry, 0.25).

% --- SECTION 7: RECOMMENDATIONS ---
recommendation(rec1, 'Reform the $25,000 offset to adjust for inflation from 1986 levels'). 
affects_constraint(rec1, section_469).

% --- SECTION 8: OMEGA VARIABLES ---
omega_variable(omega1, empirical, 'Net impact on total housing supply following 1986 enactment').
