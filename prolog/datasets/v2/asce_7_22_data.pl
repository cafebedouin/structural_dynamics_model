% =============================================================================
% DOMAIN: ASCE 7-22 (Structural Engineering Standards)
% MISSION: Clinical Sensor Output [v3.1.7]
% =============================================================================

% 1. ENTITIES & INTERVALS
entity(asce_7_22, standard).
entity(asce_committee, consensus_body).
entity(hazard_tool, scaffold). % Authorized scaffold for Snare removal.
entity(design_professional, actor).

interval(revision_cycle, 0, 9).

% 2. EVENTS & OMEGA VARIABLES
event(committee_approval, consensus_vote, 0, [approved_revisions]).
event(public_comment, transparency_check, 2, [public_review]).
event(publication, release, 5, [hazard_tool_launch]).
event(data_mandate, enforcement, 5, [digital_requirements]).

% Omega Classification
omega_variable(judgment_scope, conceptual, 'Interpretation of sound judgment required by professionals.').
omega_variable(environmental_uncertainty, empirical, 'Future intensity of 100-year mean recurrence interval rainfall.').
omega_variable(risk_tolerance, preference, 'Selection of Risk Category based on societal consequence.').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% Mountain (■C): suppression <= 0.05, snap = 0.0
constraint_claim(structural_integrity, mountain).
constraint_metric(structural_integrity, suppression_requirement, 0.05).
constraint_metric(structural_integrity, snap_back_potential, 0.0).

% Rope (⊞C): ext <= 0.35, supp <= 0.15
constraint_claim(consensus_process, rope).
constraint_metric(consensus_process, extractiveness, 0.30).
constraint_metric(consensus_process, suppression_requirement, 0.10).

% Tangled Rope (⊠T): ext 0.36-0.65, supp 0.16-0.45
constraint_claim(liability_disclaimer, tangled_rope).
constraint_metric(liability_disclaimer, extractiveness, 0.55).
constraint_metric(liability_disclaimer, suppression_requirement, 0.40).

% Snare (⊠C): ext >= 0.66, supp >= 0.46
constraint_claim(mandatory_digital_data, snare).
constraint_metric(mandatory_digital_data, extractiveness, 0.75).
constraint_metric(mandatory_digital_data, suppression_requirement, 0.80).

% 4. RECOMMENDATIONS & VETO POINTS
recommendation(r1, 'Utilize ASCE 7 Hazard Tool as the primary digital scaffold for mandatory hazard data.').
affects_constraint(r1, mandatory_digital_data).

recommendation(r2, 'Reform the liability disclaimer to acknowledge shared responsibility.').
affects_constraint(r2, liability_disclaimer).

veto_actor(asce_committee).
veto_exposed(asce_committee, r2).

% 5. MEASUREMENTS [Agency, Stability, Utility, Resilience]
measurement(0, [0.85, 0.90, 0.70, 0.75]).
measurement(9, [0.95, 0.92, 0.98, 0.90]).

% 6. INTENT EVIDENCE
intent_fact(power_delta, asce_7_22, design_professional, 0.60).
intent_fact(beneficiary, public_safety, resilience_optimization).
