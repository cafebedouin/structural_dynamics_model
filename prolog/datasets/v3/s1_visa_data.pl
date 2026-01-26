% --- SECTION 1: ENTITIES & INTERVALS ---
entity(visa_inc, organizational). % [cite: 17, 18]
entity(sec_registration, structural). % [cite: 14, 16]
entity(cert_incorporation, structural). % Exhibit 3.1 [cite: 110]
entity(loss_sharing_agreement, structural). % Exhibit 10.15 
entity(executive_class, class). % Saunders, Pollitt, etc. [cite: 82]

% T=0: Initial Filing (Nov 2007)
% T=10: Amendment 4 (Feb 2008)
% T=20: Amendment 6 (Mar 13, 2008) [cite: 4, 77]
interval(ipo_process, 0, 20).

% --- SECTION 2: EVENTS ---
event(e1, amendment_6_filing, 20, [purpose('File Exhibit 3.1')]). % [cite: 58]

% --- SECTION 3: CONSTRAINT CLAIMS & METRICS (T=20) ---
% The SEC Registration is claimed as a mandatory Mountain.
constraint_claim(sec_registration, mountain).
constraint_metric(sec_registration, extractiveness, 0.04). % Assumption: Compliance overhead is non-extractive.
constraint_metric(sec_registration, suppression_requirement, 0.04).
constraint_metric(sec_registration, snap_back_potential, 0.0).

% Loss Sharing Agreement exhibits "Snare" characteristics (Capture by legacy members).
constraint_claim(loss_sharing_agreement, rope).
constraint_metric(loss_sharing_agreement, extractiveness, 0.75). % Assumption: Significant liability shifting to the new entity.
constraint_metric(loss_sharing_agreement, suppression_requirement, 0.80).

% --- SECTION 4: TEMPORAL MEASUREMENTS (PAIRED) ---
% SEC Registration: Remains a stable Mountain/Rope.
measurement(m1, sec_registration, extractiveness, 0, 0.02).
measurement(m2, sec_registration, suppression_requirement, 0, 0.02).
measurement(m3, sec_registration, extractiveness, 20, 0.04).
measurement(m4, sec_registration, suppression_requirement, 20, 0.04).

% Loss Sharing Agreement: Evolution from functional coordination (Rope) to Snare.
measurement(m5, loss_sharing_agreement, extractiveness, 0, 0.30).
measurement(m6, loss_sharing_agreement, suppression_requirement, 0, 0.25).
measurement(m7, loss_sharing_agreement, extractiveness, 10, 0.55).
measurement(m8, loss_sharing_agreement, suppression_requirement, 10, 0.50).
measurement(m9, loss_sharing_agreement, extractiveness, 20, 0.75). % Transformation: Snare 
measurement(m10, loss_sharing_agreement, suppression_requirement, 20, 0.80).

% --- SECTION 5: DEPENDENCIES ---
% SEC effectiveness is blocked until Exhibit 3.1 is filed.
affects_constraint(cert_incorporation, sec_registration). % [cite: 58, 110]
affects_constraint(loss_sharing_agreement, visa_inc). % Liability impact 

% --- SECTION 6: INTENT EVIDENCE (REPAIRED) ---
% REPAIR: Matching 'AlternativeText' to resolve Logic Error.
intent_viable_alternative(ipo_process, visa_inc, 'Delay filing of Amended Certificate').
intent_alternative_rejected(ipo_process, visa_inc, 'Delay filing of Amended Certificate'). % [cite: 51, 58]

intent_beneficiary_class(ipo_process, executive_class).
intent_power_change(ipo_process, executive_class, 0.70). % Based on Equity Incentive Plan [cite: 110]

% --- SECTION 7: RECOMMENDATIONS ---
recommendation(rec1, 'Approve Amendment 6 to clear the Mountain constraint').
affects_constraint(rec1, sec_registration).

% --- SECTION 8: OMEGA VARIABLES ---
omega_variable(om1, empirical, 'Final settlement amount for Interchange Litigation'). %
