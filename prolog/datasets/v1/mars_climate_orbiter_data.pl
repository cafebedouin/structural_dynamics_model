% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Mars Climate Orbiter (MCO) Failure
% Domain: Systems Engineering - Software Unit Mismatch and Project Management
% ==========================================================

% --- 1. Entities & Intervals ---
entity('nasa_management', 'organizational').
entity('jet_propulsion_lab', 'organizational').
entity('lockheed_martin_astronautics', 'organizational').
entity('navigation_team', 'class').
entity('mco_spacecraft', 'individual').
entity('faster_better_cheaper_policy', 'structural').

interval('mco_mission_duration', 0, 100).

% --- 2. Events ---
event('ev01_launch', 'kinetic_initiation', 0, [date('1998-12-11'), objective('mars_exploration')]).
event('ev02_software_handoff', 'error_initiation', 10, [actor('lockheed_martin'), system('sm_forces'), error('imperial_units')]).
event('ev03_trajectory_discrepancy', 'measurement_conflict', 60, [actor('navigation_team'), symptom('angular_momentum_desaturation')]).
event('ev04_mars_insertion', 'failure_point', 98, [date('1999-09-23'), altitude('57_km'), result('disintegration')]).

% --- 3. Constraint Claims & Metrics ---
constraint_claim('mars_atmospheric_limit', 'mountain').
constraint_metric('mars_atmospheric_limit', 'accessibility_collapse', 1.0) .

constraint_claim('metric_standard_mandate', 'snare').
constraint_metric('metric_standard_mandate', 'stakes_inflation', 0.95) .

constraint_claim('fbc_philosophy_bias', 'piton').
constraint_metric('fbc_philosophy_bias', 'suppression', 0.85) .

constraint_claim('software_interface_binding', 'rope').
constraint_metric('software_interface_binding', 'suppression', 0.75) .

% --- 4. Recommendations & Veto Structure ---
recommendation('rec01', 'Adopt mandatory Independent Verification and Validation (IV&V) for all cross-organizational software interfaces.').
recommendation('rec02', 'Implement automated unit-type checking in propulsion data telemetry to bypass human calculation error.').

affects_constraint('rec01', 'fbc_philosophy_bias').
affects_constraint('rec02', 'software_interface_binding').

veto_actor('nasa_management').
veto_actor('lockheed_martin_astronautics').

veto_exposed('nasa_management', 'rec01').
veto_exposed('lockheed_martin_astronautics', 'rec02').

% --- 5. Measurements (32 Facts) ---

% Time T0 (Launch / High Optimism)
measurement('m01', 'navigation_team', 'accessibility_collapse'('individual'), 0, 0.20) .
measurement('m02', 'navigation_team', 'stakes_inflation'('individual'), 0, 0.40) .
measurement('m03', 'navigation_team', 'suppression'('individual'), 0, 0.15) .
measurement('m04', 'navigation_team', 'resistance'('individual'), 0, 0.10) .

measurement('m05', 'jet_propulsion_lab', 'accessibility_collapse'('organizational'), 0, 0.10) .
measurement('m06', 'jet_propulsion_lab', 'stakes_inflation'('organizational'), 0, 0.25) .
measurement('m07', 'jet_propulsion_lab', 'suppression'('organizational'), 0, 0.10) .
measurement('m08', 'jet_propulsion_lab', 'resistance'('organizational'), 0, 0.05) .

measurement('m09', 'nasa_management', 'accessibility_collapse'('class'), 0, 0.05) .
measurement('m10', 'nasa_management', 'stakes_inflation'('class'), 0, 0.30) .
measurement('m11', 'nasa_management', 'suppression'('class'), 0, 0.05) .
measurement('m12', 'nasa_management', 'resistance'('class'), 0, 0.00) .

measurement('m13', 'faster_better_cheaper_policy', 'accessibility_collapse'('structural'), 0, 0.00) .
measurement('m14', 'faster_better_cheaper_policy', 'stakes_inflation'('structural'), 0, 0.10) .
measurement('m15', 'faster_better_cheaper_policy', 'suppression'('structural'), 0, 0.00) .
measurement('m16', 'faster_better_cheaper_policy', 'resistance'('structural'), 0, 0.00) .

% Time Tn (Arrival / Mission Failure)
measurement('m17', 'navigation_team', 'accessibility_collapse'('individual'), 100, 0.95) .
measurement('m18', 'navigation_team', 'stakes_inflation'('individual'), 100, 1.00) .
measurement('m19', 'navigation_team', 'suppression'('individual'), 100, 0.90) .
measurement('m20', 'navigation_team', 'resistance'('individual'), 100, 0.80) .

measurement('m21', 'jet_propulsion_lab', 'accessibility_collapse'('organizational'), 100, 1.00) .
measurement('m22', 'jet_propulsion_lab', 'stakes_inflation'('organizational'), 100, 1.00) .
measurement('m23', 'jet_propulsion_lab', 'suppression'('organizational'), 100, 1.00) .
measurement('m24', 'jet_propulsion_lab', 'resistance'('organizational'), 100, 0.00) .

measurement('m25', 'nasa_management', 'accessibility_collapse'('class'), 100, 0.05) .
measurement('m26', 'nasa_management', 'stakes_inflation'('class'), 100, 0.95) .
measurement('m27', 'nasa_management', 'suppression'('class'), 100, 0.20) .
measurement('m28', 'nasa_management', 'resistance'('class'), 100, 0.95) .

measurement('m29', 'faster_better_cheaper_policy', 'accessibility_collapse'('structural'), 100, 0.00) .
measurement('m30', 'faster_better_cheaper_policy', 'stakes_inflation'('structural'), 100, 0.50) .
measurement('m31', 'faster_better_cheaper_policy', 'suppression'('structural'), 100, 0.00) .
measurement('m32', 'faster_better_cheaper_policy', 'resistance'('structural'), 100, 0.00) .

% --- 6. Intent Evidence ---
intent_viable_alternative('mco_mission_duration', 'faster_better_cheaper_policy', 'Rigorous_Unit_Validation_Protocols').
intent_alternative_rejected('mco_mission_duration', 'faster_better_cheaper_policy', 'Rigorous_Unit_Validation_Protocols').

intent_beneficiary_class('mco_mission_duration', 'nasa_management').
intent_power_change('mco_mission_duration', 'nasa_management', 0.30).

intent_loser_class('mco_mission_duration', 'navigation_team').
intent_power_change('mco_mission_duration', 'navigation_team', -0.85).

intent_suppression_level('mco_mission_duration', 'nasa_management', 'structural', 0.0).
intent_resistance_level('mco_mission_duration', 'nasa_management', 'structural', 0.0).

intent_norm_strength('mco_mission_duration', 0, 0.90) .
intent_norm_strength('mco_mission_duration', 100, 0.05).

