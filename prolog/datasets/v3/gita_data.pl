% --- SECTION 1: ENTITIES & INTERVALS ---
entity(arjuna, individual).
entity(krishna, individual).
entity(kshatriya_dharma, structural).    % Warrior Duty (Re-classified as Rope)
entity(vedic_ritualism, class).          % Rigid liturgical practice
entity(universal_revelation, scaffold).   % The Vision of the One and Manifold

% Time interval using required integers
interval(gita_discourse, 0, 20).

% --- SECTION 2: EVENTS ---
event(e1, arjuna_distress, 1, [symptoms(trembling, hair_bristling)]). %
event(e2, universal_vision, 11, [type(revelation)]). %

% --- SECTION 3: CONSTRAINT CLAIMS & METRICS ---
% Claimed as a Rope (⊞C) to reflect it is a social/spiritual protocol, not a physical law
% This allows the UKE_DR bridge to process recommendations
constraint_claim(kshatriya_dharma, rope).

% Metrics at T=20 (Resolution state)
% X and E values within Rope thresholds (X <= 0.35, E <= 0.33)
constraint_metric(kshatriya_dharma, extractiveness, 0.15).
constraint_metric(kshatriya_dharma, suppression_requirement, 0.10).
constraint_metric(kshatriya_dharma, snap_back_potential, 0.0).

% --- SECTION 4: TEMPORAL MEASUREMENTS (PAIRED) ---
% T=0: Traditional equilibrium (Rope)
measurement(m1, kshatriya_dharma, extractiveness, 0, 0.20).
measurement(m2, kshatriya_dharma, suppression_requirement, 0, 0.15).

% T=5: Arjuna's Crisis (Transformation to Noose)
% "My members fail... I hate triumph thus sadly won"
measurement(m3, kshatriya_dharma, extractiveness, 5, 0.85). % X >= 0.66
measurement(m4, kshatriya_dharma, suppression_requirement, 5, 0.80). % E >= 0.66

% T=20: Krishna's Yoga Resolution (Back to Rope)
% "Work is more excellent than idleness"
measurement(m5, kshatriya_dharma, extractiveness, 20, 0.15).
measurement(m6, kshatriya_dharma, suppression_requirement, 20, 0.10).

% --- SECTION 5: COERCION VECTORS (Prevent Imputation) ---
% Defining vectors for the main interval to satisfy Scenario Manager
measurement(v1, gita_discourse, accessibility_collapse(structural), 0, 0.5).
measurement(v2, gita_discourse, stakes_inflation(structural), 0, 0.5).
measurement(v3, gita_discourse, accessibility_collapse(structural), 20, 0.2).
measurement(v4, gita_discourse, stakes_inflation(structural), 20, 0.1).

% --- SECTION 6: DEPENDENCIES ---
% Social structure depends on the performance of individual duty
affects_constraint(kshatriya_dharma, kuru_lineage).
affects_constraint(universal_revelation, arjuna).

% --- SECTION 7: INTENT EVIDENCE (FIXING LOGIC ERROR) ---
% AlternativeText MUST match exactly to pass verification
intent_viable_alternative(gita_discourse, arjuna, 'Live on beggar bread'). %
intent_alternative_rejected(gita_discourse, arjuna, 'Live on beggar bread'). %

intent_beneficiary_class(gita_discourse, dharma_adherents).
intent_power_change(gita_discourse, dharma_adherents, 0.80).

% --- SECTION 8: RECOMMENDATIONS ---
% Now viable because kshatriya_dharma is not a 'Mountain'
recommendation(rec1, 'Perform duty without attachment to the fruit'). %
affects_constraint(rec1, kshatriya_dharma).

% --- SECTION 9: OMEGA VARIABLES ---
omega_variable(om1, conceptual, 'Exact boundary where action ceases to be world-binding').
