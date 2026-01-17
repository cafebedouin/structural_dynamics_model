% ==========================================================
% v3.1 Structural Analysis Engine - Data Pack: Cytokine Storm
% Domain: Biological Science - Nature's Narrative (Non-Conscious Coercion)
% ==========================================================

% --- 1. Entities & Intervals ---
entity('viral_population', 'individual').
entity('innate_immune_system', 'organizational').
entity('host_tissue_class', 'class').
entity('evolutionary_selection', 'class').
entity('systemic_homeostasis', 'structural').

interval('cytokine_storm_cycle', 0, 100).

% --- 2. Events ---
event('ev01_viral_entry', 'kinetic_initiation', 5, [mechanism('endocytosis'), target('epithelial_cells')]).
event('ev02_replication_surge', 'threshold_crossing', 30, [metric('viral_load'), state('exponential_growth')]).
event('ev03_signaling_loop', 'positive_feedback', 65, [mediator('il_6'), mediator('tnf_alpha'), result('hyperinflammation')]).
event('ev04_systemic_collapse', 'finality', 98, [state('organ_failure'), result('host_death')]).

% --- 3. Constraint Claims & Metrics ---
constraint_claim('apoptosis_threshold_limit', 'mountain').
constraint_metric('apoptosis_threshold_limit', 'accessibility_collapse', 1.0) .

constraint_claim('replication_velocity_noose', 'noose').
constraint_metric('replication_velocity_noose', 'stakes_inflation', 0.98) .

constraint_claim('innate_response_bias', 'zombie').
constraint_metric('innate_response_bias', 'suppression', 0.85) .

constraint_claim('receptor_binding_tether', 'rope').
constraint_metric('receptor_binding_tether', 'suppression', 0.70) .

% --- 4. Recommendations & Veto Structure ---
recommendation('rec01', 'Deploy targeted monoclonal antibodies to block specific cytokine signaling pathways.').
recommendation('rec02', 'Induce systemic cooling and metabolic slowing to raise the mountain of cellular death.').

affects_constraint('rec01', 'receptor_binding_tether').
affects_constraint('rec02', 'apoptosis_threshold_limit').

veto_actor('innate_immune_system').
veto_exposed('innate_immune_system', 'rec01').

% --- 5. Measurements (32 Facts) ---

% Time T0 (Initial Infection)
measurement('m01', 'viral_population', 'accessibility_collapse'('individual'), 0, 0.20) .
measurement('m02', 'viral_population', 'stakes_inflation'('individual'), 0, 1.00) .
measurement('m03', 'viral_population', 'suppression'('individual'), 0, 0.40) .
measurement('m04', 'viral_population', 'resistance'('individual'), 0, 0.05) .

measurement('m05', 'innate_immune_system', 'accessibility_collapse'('organizational'), 0, 0.10) .
measurement('m06', 'innate_immune_system', 'stakes_inflation'('organizational'), 0, 0.20) .
measurement('m07', 'innate_immune_system', 'suppression'('organizational'), 0, 0.05) .
measurement('m08', 'innate_immune_system', 'resistance'('organizational'), 0, 0.00) .

measurement('m09', 'host_tissue_class', 'accessibility_collapse'('class'), 0, 0.05) .
measurement('m10', 'host_tissue_class', 'stakes_inflation'('class'), 0, 0.10) .
measurement('m11', 'host_tissue_class', 'suppression'('class'), 0, 0.00) .
measurement('m12', 'host_tissue_class', 'resistance'('class'), 0, 0.00) .

measurement('m13', 'systemic_homeostasis', 'accessibility_collapse'('structural'), 0, 0.00) .
measurement('m14', 'systemic_homeostasis', 'stakes_inflation'('structural'), 0, 0.05) .
measurement('m15', 'systemic_homeostasis', 'suppression'('structural'), 0, 0.00) .
measurement('m16', 'systemic_homeostasis', 'resistance'('structural'), 0, 0.00) .

% Time Tn (Terminal Organ Failure)
measurement('m17', 'viral_population', 'accessibility_collapse'('individual'), 100, 0.00) .
measurement('m18', 'viral_population', 'stakes_inflation'('individual'), 100, 1.00) .
measurement('m19', 'viral_population', 'suppression'('individual'), 100, 0.00) .
measurement('m20', 'viral_population', 'resistance'('individual'), 100, 0.95) .

measurement('m21', 'innate_immune_system', 'accessibility_collapse'('organizational'), 100, 1.00) .
measurement('m22', 'innate_immune_system', 'stakes_inflation'('organizational'), 100, 1.00) .
measurement('m23', 'innate_immune_system', 'suppression'('organizational'), 100, 1.00) .
measurement('m24', 'innate_immune_system', 'resistance'('organizational'), 100, 0.00) .

measurement('m25', 'host_tissue_class', 'accessibility_collapse'('class'), 100, 1.00) .
measurement('m26', 'host_tissue_class', 'stakes_inflation'('class'), 100, 1.00) .
measurement('m27', 'host_tissue_class', 'suppression'('class'), 100, 0.98) .
measurement('m28', 'host_tissue_class', 'resistance'('class'), 100, 0.02) .

measurement('m29', 'systemic_homeostasis', 'accessibility_collapse'('structural'), 100, 0.00) .
measurement('m30', 'systemic_homeostasis', 'stakes_inflation'('structural'), 100, 0.40) .
measurement('m31', 'systemic_homeostasis', 'suppression'('structural'), 100, 0.00) .
measurement('m32', 'systemic_homeostasis', 'resistance'('structural'), 100, 0.00) .

% --- 6. Intent Evidence ---
intent_viable_alternative('cytokine_storm_cycle', 'systemic_homeostasis', 'Targeted_Immunomodulation').
intent_alternative_rejected('cytokine_storm_cycle', 'systemic_homeostasis', 'Targeted_Immunomodulation').

intent_beneficiary_class('cytokine_storm_cycle', 'evolutionary_selection').
intent_power_change('cytokine_storm_cycle', 'evolutionary_selection', 0.95) .

intent_loser_class('cytokine_storm_cycle', 'host_tissue_class').
intent_power_change('cytokine_storm_cycle', 'host_tissue_class', -1.00) .

intent_suppression_level('cytokine_storm_cycle', 'evolutionary_selection', 'structural', 0.0).
intent_resistance_level('cytokine_storm_cycle', 'evolutionary_selection', 'structural', 0.0).

intent_norm_strength('cytokine_storm_cycle', 0, 0.95).
intent_norm_strength('cytokine_storm_cycle', 100, 0.05).
