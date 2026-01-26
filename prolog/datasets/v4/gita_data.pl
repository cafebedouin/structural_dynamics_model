% ==========================================
% SECTION 1: ENTITIES & INTERVALS
% ==========================================
entity(arjuna, individual).
entity(krishna, individual).
entity(atman, structural).      % The Indestructible Soul
entity(dharma_varna, scaffold). % Social/Warrior Duty
entity(ahamkara, class).        % The Ego/Attachment to results
entity(aswattha_tree, structural). % The manifestation of sense-life

interval(discourse_period, 0, 30). % T=0 (Grief), T=15 (Vision), T=30 (Action)

% ==========================================
% SECTION 2: EVENTS
% ==========================================
event(e1, paralysis_of_will, 0, [location(kurukshetra)]).
event(e2, viswarupa_revelation, 15, [intensity(1.0)]).
event(e3, resolve_to_fight, 30, [status(complete)]).

% ==========================================
% SECTION 3: CONSTRAINT CLAIMS & METRICS (T_end)
% ==========================================
% The Atman is claimed as a Mountain (Natural Law)
constraint_claim(atman, mountain).
constraint_metric(atman, extractiveness, 0.0).
constraint_metric(atman, suppression_requirement, 0.05).
constraint_metric(atman, resistance, 0.05).

% Arjuna's grief is exposed as a Snare (Constructed Constraint)
constraint_claim(arjuna_grief, snare).
constraint_metric(arjuna_grief, extractiveness, 0.85).
constraint_metric(arjuna_grief, suppression_requirement, 0.90).
constraint_metric(arjuna_grief, resistance, 0.80).

% ==========================================
% SECTION 4: TEMPORAL MEASUREMENTS (Signature Detection)
% ==========================================

% --- Signature 1: Natural Law (The Atman) ---
% Stable, low suppression, no resistance, no alternatives.
measurement(m1, atman, extractiveness, 0, 0.0).
measurement(m2, atman, suppression_requirement, 0, 0.05).
measurement(m3, atman, resistance, 0, 0.05).
measurement(m4, atman, extractiveness, 30, 0.0).
measurement(m5, atman, suppression_requirement, 30, 0.05).
measurement(m6, atman, resistance, 30, 0.05).

% --- Signature 2: Coordination Scaffold (Dharma/Varna) ---
% High accessibility, voluntary adoption, HAD alternatives (Arjuna's refusal).
measurement(m7, dharma_varna, extractiveness, 0, 0.15).
measurement(m8, dharma_varna, suppression_requirement, 0, 0.10).
measurement(m9, dharma_varna, resistance, 0, 0.70). % Initial resistance from Arjuna
measurement(m10, dharma_varna, extractiveness, 30, 0.15).
measurement(m11, dharma_varna, suppression_requirement, 30, 0.10).
measurement(m12, dharma_varna, resistance, 30, 0.10). % Resistance drops after understanding

% --- Signature 3: Constructed Constraint (Ahamkara/Attachment) ---
% High extraction and resistance; evolves from "Rope" to "Snare" in perception.
measurement(m13, ahamkara, extractiveness, 0, 0.40).
measurement(m14, ahamkara, suppression_requirement, 0, 0.35).
measurement(m15, ahamkara, resistance, 0, 0.30).
measurement(m16, ahamkara, extractiveness, 30, 0.85). % Revealed as highly extractive
measurement(m17, ahamkara, suppression_requirement, 30, 0.80).
measurement(m18, ahamkara, resistance, 30, 0.75).

% ==========================================
% SECTION 5: VIABLE ALTERNATIVES (Signature Discriminators)
% ==========================================
% For Atman: NO alternatives exist (Signature: Natural Law)

% For Dharma: Alternatives existed and were rejected (Signature: Coordination Scaffold)
intent_viable_alternative(discourse_period, dharma_varna, 'Renunciation and begging for bread').
intent_alternative_rejected(discourse_period, dharma_varna, 'Renunciation and begging for bread').
intent_viable_alternative(discourse_period, dharma_varna, 'Flight from the battlefield').
intent_alternative_rejected(discourse_period, dharma_varna, 'Flight from the battlefield').

% ==========================================
% SECTION 6: DEPENDENCIES & INTENT
% ==========================================
affects_constraint(atman, dharma_varna). % Duty flows from the nature of the Soul
affects_constraint(ahamkara, arjuna_grief). % Attachment causes the distress

% Beneficiary of understanding
intent_beneficiary_class(discourse_period, arjuna).
intent_power_change(discourse_period, arjuna, 0.90). % Mastery of self

% ==========================================
% SECTION 7: RECOMMENDATIONS
% ==========================================
recommendation(r1, 'Sever the roots of the Aswattha tree with the axe of detachment').
affects_constraint(r1, aswattha_tree).

recommendation(r2, 'Abandon attachment to the fruit of action (Tyaga)').
affects_constraint(r2, ahamkara).
