% =============================================================================
% v3.1 Structural Analysis Engine - Data Pack: 26 U.S. Code § 469
% Domain: Federal Tax Law - Passive Activity Loss and Credit Limitations
% Interval: Tax Reform Act Implementation Period (T0: 1986, Tn: 2000) 
% =============================================================================

% --- 1. Entities & Intervals ---
entity(taxpayer_individual, individual).
entity(internal_revenue_service, organizational).
entity(passive_investor_class, class).
entity(real_estate_professional_class, class). 
entity(us_treasury_system, structural).

interval(tra86_regulatory_cycle, 1986, 2000). 

% --- 2. Events ---
event(ev01_tra86_enactment, legislation_enactment, 1986, [statute("26 USC 469"), effect(disallowance)]). 
event(ev02_participation_tests, rule_definition, 1987, [criteria(regular_continuous_substantial)]). 
event(ev03_phase_in_decrement, systemic_shift, 1988, [percentage(40)]). 
event(ev04_real_estate_relief, amendment, 1993, [provision(subsection_c7), target(real_estate_professionals)]). 

% --- 3. Constraint Claims & Metrics ---
% Mountain: The absolute statutory ceiling on loss offsets. A mathematical boundary.
constraint_claim(statutory_loss_limit, mountain). 
constraint_metric(statutory_loss_limit, accessibility_collapse, 0.95).

% Noose: The Material Participation Tests. A closing circle of criteria (regular, continuous, substantial).
constraint_claim(material_participation_noose, noose). 
constraint_metric(material_participation_noose, stakes_inflation, 0.90).

% Zombie: The 1986 Transition Rules. Legacy phase-in percentages that controlled behavior while dying out.
constraint_claim(pre_enactment_phase_in, zombie). 
constraint_metric(pre_enactment_phase_in, suppression, 0.70).

% --- 4. Recommendations & Veto Structure ---
recommendation(rec01, 'Allow broad grouping of unrelated business activities to meet the 500-hour participation floor.'). 
recommendation(rec02, 'Expand the $25,000 rental loss allowance to taxpayers above the $150,000 adjusted gross income ceiling.'). 

affects_constraint(rec01, material_participation_noose).
affects_constraint(rec02, statutory_loss_limit).

veto_actor(us_treasury_system).
veto_exposed(us_treasury_system, rec01).
veto_exposed(us_treasury_system, rec02).

% --- 5. Measurements (32 Facts: A, S, U, R across 4 levels at T0 and Tn) ---

% Time T0 (1986: Pre-Enactment / Initial Adoption)
measurement(m01, taxpayer_individual, accessibility_collapse(individual), 1986, 0.15).
measurement(m02, taxpayer_individual, stakes_inflation(individual), 1986, 0.20).
measurement(m03, taxpayer_individual, suppression(individual), 1986, 0.10).
measurement(m04, taxpayer_individual, resistance(individual), 1986, 0.05).

measurement(m05, internal_revenue_service, accessibility_collapse(organizational), 1986, 0.25).
measurement(m06, internal_revenue_service, stakes_inflation(organizational), 1986, 0.30).
measurement(m07, internal_revenue_service, suppression(organizational), 1986, 0.10).
measurement(m08, internal_revenue_service, resistance(organizational), 1986, 0.00).

measurement(m09, passive_investor_class, accessibility_collapse(class), 1986, 0.20).
measurement(m10, passive_investor_class, stakes_inflation(class), 1986, 0.40).
measurement(m11, passive_investor_class, suppression(class), 1986, 0.20).
measurement(m12, passive_investor_class, resistance(class), 1986, 0.85).

measurement(m13, us_treasury_system, accessibility_collapse(structural), 1986, 0.05).
measurement(m14, us_treasury_system, stakes_inflation(structural), 1986, 0.10).
measurement(m15, us_treasury_system, suppression(structural), 1986, 0.00). % Beneficiary
measurement(m16, us_treasury_system, resistance(structural), 1986, 0.00). % Beneficiary

% Time Tn (2000: Full Compliance / Post-Transition)
measurement(m17, taxpayer_individual, accessibility_collapse(individual), 2000, 0.90).
measurement(m18, taxpayer_individual, stakes_inflation(individual), 2000, 1.00).
measurement(m19, taxpayer_individual, suppression(individual), 2000, 0.95).
measurement(m20, taxpayer_individual, resistance(individual), 2000, 0.10).

measurement(m21, internal_revenue_service, accessibility_collapse(organizational), 2000, 0.05).
measurement(m22, internal_revenue_service, stakes_inflation(organizational), 2000, 0.95).
measurement(m23, internal_revenue_service, suppression(organizational), 2000, 0.05).
measurement(m24, internal_revenue_service, resistance(organizational), 2000, 0.98).

measurement(m25, passive_investor_class, accessibility_collapse(class), 2000, 0.95).
measurement(m26, passive_investor_class, stakes_inflation(class), 2000, 0.90).
measurement(m27, passive_investor_class, suppression(class), 2000, 0.90).
measurement(m28, passive_investor_class, resistance(class), 2000, 0.20).

measurement(m29, us_treasury_system, accessibility_collapse(structural), 2000, 0.00).
measurement(m30, us_treasury_system, stakes_inflation(structural), 2000, 0.50).
measurement(m31, us_treasury_system, suppression(structural), 2000, 0.00). % Beneficiary
measurement(m32, us_treasury_system, resistance(structural), 2000, 0.00). % Beneficiary

% --- 6. Intent Evidence ---
intent_viable_alternative(tra86_regulatory_cycle, us_treasury_system, 'Pro_Rata_Loss_Deduction_Standard').
intent_alternative_rejected(tra86_regulatory_cycle, us_treasury_system, 'Pro_Rata_Loss_Deduction_Standard').

intent_beneficiary_class(tra86_regulatory_cycle, us_treasury_system).
intent_power_change(tra86_regulatory_cycle, us_treasury_system, 0.75). % Massive structural capture of previous shelters

intent_loser_class(tra86_regulatory_cycle, passive_investor_class).
intent_power_change(tra86_regulatory_cycle, passive_investor_class, -0.85). % Liquidation of passive activity shelters

intent_suppression_level(tra86_regulatory_cycle, us_treasury_system, structural, 0.0).
intent_resistance_level(tra86_regulatory_cycle, us_treasury_system, structural, 0.0).

intent_norm_strength(tra86_regulatory_cycle, 1986, 0.30). % Law is new/contested
intent_norm_strength(tra86_regulatory_cycle, 2000, 0.98). % Absolute standard for tax accounting
