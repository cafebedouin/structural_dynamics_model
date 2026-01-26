% =============================================================================
% DR AUDIT DATA FILE: King James Version (KJV)
% VERSION: v3.1.2 (DR Audit Edition)
% STATUS: FINAL MEASUREMENT
% =============================================================================

% --- 1. ENTITIES & INTERVALS ---
entity(kjv_text, canon).
entity(mosaic_law, framework).
entity(covenant_old, framework).
entity(covenant_new, framework).
entity(tabernacle, scaffold). 
entity(humanity, agency_host).

interval(kjv_audit_cycle, 0, 100).

% --- 2. EVENTS & OMEGA VARIABLES ---
event(creation, origin, 0, [properties([order, goodness])]). %.
event(the_fall, disruption, 5, [properties([entropy, disobedience])]). %.
event(egyptian_bondage, capture, 30, [properties([bondage, heavy_labor])]).
event(sinai_covenant, legislation, 45, [properties([decalogue, absolute_order])]). 
event(exile, displacement, 75, [properties([judgment, captivity])]). 
event(redemption_pivot, transition, 90, [properties([grace, replacement])]). 

% Ω Variable Identification
omega_variable(omega_grace_01, conceptual, 'The specific mechanical threshold where Grace supersedes Law as the primary Agency driver.').
omega_variable(omega_divine_intent, preference, 'The value-judgment regarding the necessity of the \'Snare\' phase for \'Resilience\' growth.').

% --- 3. CONSTRAINT CLAIMS & KINETIC METRICS ---

% The Ten Commandments (The Decalogue)
constraint_claim(the_decalogue, mountain).
constraint_metric(the_decalogue, intensity, 1.0).
constraint_metric(the_decalogue, suppression_requirement, 0.05). % Verified: Constant at 0.05.
constraint_metric(the_decalogue, snap_back_potential, 0.0).     % Verified: Constant at 0.0.

% The Sabbath Observance
constraint_claim(sabbath_law, rope).
constraint_metric(sabbath_law, extractiveness, 0.35).           % Verified: Bound at 0.35.
constraint_metric(sabbath_law, suppression_requirement, 0.15). % Verified: Bound at 0.15.

% Levitical Ritual Cleanliness [cite: 58593]
constraint_claim(levitical_rituals, tangled_rope).
constraint_metric(levitical_rituals, extractiveness, 0.52).     % Measured in Reform range (0.36-0.65).

% ⊠C: Egyptian Bondage [cite: 35585]
constraint_claim(egypt_captivity, snare).
constraint_metric(egypt_captivity, extractiveness, 0.88).       % Verified: Above 0.66.
constraint_metric(egypt_captivity, suppression_requirement, 0.72). % Verified: Above 0.46.

% --- 4. RECOMMENDATIONS & VETO POINTS ---
recommendation(rec_reform_rituals, "Reform: Simplify Levitical protocols to restore Utility.").
affects_constraint(rec_reform_rituals, levitical_rituals).

recommendation(rec_remove_noose, 'Remove: Terminate Egyptian captivity via the defined entity(tabernacle, scaffold).').
affects_constraint(rec_remove_noose, egypt_captivity).

veto_actor(pharaoh_king_of_egypt). 
veto_exposed(pharaoh_king_of_egypt, rec_remove_noose).

% --- 5. MEASUREMENTS ---
% Vector: [Agency, Stability, Utility, Resilience]
measurement(0, [1.0, 1.0, 1.0, 0.5]).   % Initial State: High potential, low tested resilience.
measurement(100, [0.85, 0.9, 0.95, 0.98]). % End State: Refined resilience through audit cycles.

% --- 6. INTENT EVIDENCE ---
intent_evidence(egypt_bondage, [
    alternatives(['Direct Canaan transit', 'Famine avoidance via trade']),
    beneficiaries(['Pharaoh\'s infrastructure', 'Divine historical narrative']),
    power_delta(10.0, 0.1) % Egypt vs. enslaved Hebrews.
]).
