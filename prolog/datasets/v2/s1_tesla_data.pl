% 1. ENTITIES & INTERVALS
entity(tesla_motors, corporation).
entity(fremont_facility, scaffold). [cite_start]% Transition from outsourced gliders to in-house manufacturing[cite: 10, 39, 46].
entity(doe_loan_facility, capital_source).
entity(elon_musk, lead_actor).
entity(model_s_program, development_track).

interval(inception_to_s1, 0, 8). [cite_start]% 2003 to 2011[cite: 1, 6].
interval(model_s_ramp, 8, 10). [cite_start]% 2011 to 2013 expected production[cite: 4, 38].

% 2. EVENTS & OMEGA VARIABLES
event(incorporation, formation, 0, [location(delaware)]). [cite_start]% 2003[cite: 6].
event(roadster_launch, commercialization, 5, [units_delivered(1650)]). [cite_start]% 2008[cite: 6, 46].
event(doe_loan_closing, capitalization, 7, [amount(465000000)]). [cite_start]% Jan 2010[cite: 7, 35].
event(nummi_purchase, acquisition, 7, [location(fremont), price(48500000)]). [cite_start]% Oct 2010[cite: 39, 55].
event(s1_amendment, reporting, 8, [purpose(equity_offering)]). [cite_start]% June 2, 2011[cite: 1].

omega_variable(ev_adoption_rate, conceptual, uncertainty_regarding_market_willingness_to_adopt_electric_vehicles). [cite_start]% ΩC[cite: 6, 12].
omega_variable(model_s_cost_basis, empirical, unknown_procurement_costs_for_unfinalized_model_s_components). [cite_start]% ΩE[cite: 8, 10].

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
constraint_claim(accumulated_deficit, mountain). [cite_start]% $463.9M deficit is a physical barrier to operations[cite: 6, 34].
constraint_metric(accumulated_deficit, intensity, 0.92).
constraint_metric(accumulated_deficit, suppression_requirement, 0.85).

constraint_claim(doe_covenants, rope). [cite_start]% Draw conditions and stock ownership requirements bind management[cite: 13, 36].
constraint_metric(doe_covenants, intensity, 0.70).
constraint_metric(doe_covenants, snap_back_potential, 0.60).

constraint_claim(lotus_glider_dependency, noose). [cite_start]% Supply limited to 2,400 units; production ends Dec 2011[cite: 9, 16, 46].
constraint_metric(lotus_glider_dependency, extractiveness, 0.78). % High extraction of resources to sustain legacy platform.
constraint_metric(lotus_glider_dependency, suppression_requirement, 0.55). % Requires strict adherence to Lotus production timelines.

% ⊠C Verification: lotus_glider_dependency (extractiveness 0.78 >= 0.66 AND suppression 0.55 >= 0.46) = NOOSE verified.

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(transition_to_fremont, remove_noose_via_inhouse_manufacturing).
affects_constraint(transition_to_fremont, lotus_glider_dependency).

veto_actor(department_of_energy).
veto_exposed(department_of_energy, transition_to_fremont). [cite_start]% DOE holds security interest in Fremont facility[cite: 35, 57].

veto_actor(elon_musk).
veto_exposed(elon_musk, s1_amendment). [cite_start]% 29.1% ownership gives significant control[cite: 52].

% 5. MEASUREMENTS
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [0.10, 0.10, 0.10, 0.10]). % Inception.
measurement(5, [0.45, 0.25, 0.60, 0.35]). [cite_start]% Roadster launch; high loss rate limits stability[cite: 11, 23].
measurement(8, [0.65, 0.30, 0.85, 0.70]). % S-1 Filing; [cite_start]DOE backing provides resilience despite mountain of debt[cite: 34, 35].

% 6. INTENT EVIDENCE
intent_evidence(internal_combustion_dominance, early_adopter_consumers, disrupt_legacy_auto_incumbents).
intent_evidence(outsourced_manufacturing, tesla_engineering_team, transition_to_integrated_oem).
