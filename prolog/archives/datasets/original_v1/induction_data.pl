% 1. ENTITIES & INTERVALS
entity(e_hume, 'philosopher').
entity(e_kant, 'philosopher').
entity(e_human_reason, 'cognitive_system').
entity(e_inductive_logic, 'methodological_framework').

interval(int_induction_debate, 173901, 202211).

% 2. EVENTS
event(ev_treatise_pub, 'publication', 173901, [title('A_Treatise_of_Human_Nature')]).
event(ev_critique_pure_reason, 'publication', 178101, [title('Critique_of_Pure_Reason')]).
event(ev_sep_revision, 'revision', 202211, [topic('problem_of_induction')]).

% 3. CONSTRAINT CLAIMS & METRICS
% MOUNTAIN: Static bedrock logic.
constraint_claim('the_uniformity_principle', mountain).
constraint_metric('the_uniformity_principle', intensity, 0.95).
constraint_metric('the_uniformity_principle', suppression_requirement, 0.0).
constraint_metric('the_uniformity_principle', snap_back_potential, 0.0).
constraint_metric('the_uniformity_principle', extractiveness, 0.05).

% NOOSE: High suppression/extractiveness trap.
constraint_claim('the_inductive_circularity_trap', snare).
constraint_metric('the_inductive_circularity_trap', intensity, 0.85).
constraint_metric('the_inductive_circularity_trap', suppression_requirement, 0.75).
constraint_metric('the_inductive_circularity_trap', snap_back_potential, 0.30).
constraint_metric('the_inductive_circularity_trap', extractiveness, 0.90).

% ZOMBIE: Persistent arational instinct.
constraint_claim('custom_and_habit', piton).
constraint_metric('custom_and_habit', intensity, 0.75).
constraint_metric('custom_and_habit', suppression_requirement, 0.20).
constraint_metric('custom_and_habit', snap_back_potential, 0.10).
constraint_metric('custom_and_habit', extractiveness, 0.40).

% 4. RECOMMENDATIONS & VETO STRUCTURE
recommendation(rec_meta_induction, 'adopt_meta_inductive_strategies_to_achieve_predictive_optimality').
recommendation(rec_pragmatic_vindication, 'treat_induction_as_a_necessary_posit_for_action').

affects_constraint(rec_meta_induction, 'the_inductive_circularity_trap').
affects_constraint(rec_pragmatic_vindication, 'the_inductive_circularity_trap').

veto_actor('skeptical_scholars').
veto_exposed('skeptical_scholars', rec_meta_induction).

% 5. MEASUREMENTS (Full vectors for T0 and Tn)
measurement(m_01, e_human_reason, accessibility_collapse(individual), 173901, 0.80).
measurement(m_02, e_human_reason, accessibility_collapse(organizational), 173901, 0.50).
measurement(m_03, e_human_reason, accessibility_collapse(class), 173901, 0.90).
measurement(m_04, e_human_reason, accessibility_collapse(structural), 173901, 0.95).
measurement(m_05, e_human_reason, stakes_inflation(individual), 173901, 0.60).
measurement(m_06, e_human_reason, stakes_inflation(organizational), 173901, 0.40).
measurement(m_07, e_human_reason, stakes_inflation(class), 173901, 0.85).
measurement(m_08, e_human_reason, stakes_inflation(structural), 173901, 0.90).
measurement(m_09, e_human_reason, suppression(individual), 173901, 0.40).
measurement(m_10, e_human_reason, suppression(organizational), 173901, 0.30).
measurement(m_11, e_human_reason, suppression(class), 173901, 0.70).
measurement(m_12, e_human_reason, suppression(structural), 173901, 0.80).
measurement(m_13, e_human_reason, resistance(individual), 173901, 0.20).
measurement(m_14, e_human_reason, resistance(organizational), 173901, 0.50).
measurement(m_15, e_human_reason, resistance(class), 173901, 0.10).
measurement(m_16, e_human_reason, resistance(structural), 173901, 0.05).

measurement(m_17, e_human_reason, accessibility_collapse(individual), 202211, 0.40).
measurement(m_18, e_human_reason, accessibility_collapse(organizational), 202211, 0.30).
measurement(m_19, e_human_reason, accessibility_collapse(class), 202211, 0.60).
measurement(m_20, e_human_reason, accessibility_collapse(structural), 202211, 0.75).
measurement(m_21, e_human_reason, stakes_inflation(individual), 202211, 0.50).
measurement(m_22, e_human_reason, stakes_inflation(organizational), 202211, 0.60).
measurement(m_23, e_human_reason, stakes_inflation(class), 202211, 0.70).
measurement(m_24, e_human_reason, stakes_inflation(structural), 202211, 0.80).
measurement(m_25, e_human_reason, suppression(individual), 202211, 0.30).
measurement(m_26, e_human_reason, suppression(organizational), 202211, 0.40).
measurement(m_27, e_human_reason, suppression(class), 202211, 0.50).
measurement(m_28, e_human_reason, suppression(structural), 202211, 0.60).
measurement(m_29, e_human_reason, resistance(individual), 202211, 0.70).
measurement(m_30, e_human_reason, resistance(organizational), 202211, 0.80).
measurement(m_31, e_human_reason, resistance(class), 202211, 0.65).
measurement(m_32, e_human_reason, resistance(structural), 202211, 0.40).

% 6. INTENT EVIDENCE (REPAIRED BENEFICIARY LOGIC)
intent_viable_alternative(int_induction_debate, e_human_reason, 'synthetic_a_priori').
intent_viable_alternative(int_induction_debate, e_human_reason, 'meta_induction').
intent_alternative_rejected(int_induction_debate, e_human_reason, 'synthetic_a_priori').
intent_beneficiary_class(int_induction_debate, 'skeptical_epistemologists').
intent_power_change(int_induction_debate, 'skeptical_epistemologists', 0.80).
intent_power_change(int_induction_debate, 'rationalist_philosophers', -0.55).
intent_suppression_level(int_induction_debate, 'non_inductive_methods', structural, 0.80).
intent_resistance_level(int_induction_debate, 'skeptical_class', individual, 0.90).
intent_norm_strength(int_induction_debate, 173901, 0.95).
