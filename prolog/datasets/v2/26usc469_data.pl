% =============================================================================
% DOMAIN: 26USC469 (Passive Activity Loss Rules)
% MISSION: Clinical Sensor Output [v3.1.7]
% =============================================================================

% 1. ENTITIES & INTERVALS
% -----------------------------------------------------------------------------
entity(taxpayer_subject, person).
entity(passive_activity_rules, framework).
entity(active_participation_allowance, scaffold). % Transitional relief (e.g., $25k rental allowance).
entity(irs_code_section, structure).

% Primary interval only to sync with the Scenario Manager.
interval(usc469_audit_cycle, 0, 10).

% 2. EVENTS & OMEGA VARIABLES
% -----------------------------------------------------------------------------
event(initial_investment, capital_outlay, 0, [type(rental_real_estate)]).
event(loss_incurred, financial_deficit, 3, [amount(substantial)]).
event(material_participation_test, regulatory_check, 5, [status(failed)]).
event(disallowed_loss_carryover, constraint_enforcement, 10, [status(suspended)]).

% OMEGA CLASSIFICATION
% Ω_C: Conceptual (Defining 'Material Participation')
omega_variable(omega_c_participation, conceptual, 'The threshold definition of material participation across diverse industry standards.').

% Ω_E: Empirical (Measurement of actual hours)
omega_variable(omega_e_audit_trail, empirical, 'Verifiable log of taxpayer hours dedicated solely to management vs labor.').

% 3. CONSTRAINT CLAIMS & KINETIC METRICS
% -----------------------------------------------------------------------------

% PASSIVE LOSS LIMITATION: NOOSE (ext >= 0.66, supp >= 0.46)
% High extraction of liquidity/utility; high suppression of tax-benefit realization.
constraint_claim(passive_loss_limitation, noose).
constraint_metric(passive_loss_limitation, extractiveness, 0.88).
constraint_metric(passive_loss_limitation, suppression_requirement, 0.75).

% RENTAL REAL ESTATE ALLOWANCE: ROPE (ext <= 0.35, supp <= 0.15)
% Nominally protective/coordinative relief for mid-income subjects.
constraint_claim(rental_allowance, rope).
constraint_metric(rental_allowance, extractiveness, 0.25).
constraint_metric(rental_allowance, suppression_requirement, 0.10).

% TAXPAYER FILING OBLIGATION: MOUNTAIN (supp <= 0.05, snap = 0.0)
% Invariant regulatory requirement of the environment.
constraint_claim(filing_requirement, mountain).
constraint_metric(filing_requirement, suppression_requirement, 0.04).
constraint_metric(filing_requirement, snap_back_potential, 0.0).

% 4. RECOMMENDATIONS & VETO POINTS
% -----------------------------------------------------------------------------
recommendation(rec01, 'Utilize active_participation_allowance to mitigate passive_loss_limitation.').
affects_constraint(rec01, passive_loss_limitation).

% VETO LOGIC: IRS Auditor acts as the blocking agent for non-compliant claims.
veto_actor(irs_auditor).
veto_exposed(irs_auditor, rec01). 

% 5. MEASUREMENTS [Agency, Stability, Utility, Resilience]
% -----------------------------------------------------------------------------
% T=0: Investment initiation (High Agency/Utility)
measurement(0, [0.90, 0.80, 0.85, 0.70]).

% T=10: Post-regulatory lock (Agency collapse due to suspended losses)
measurement(10, [0.15, 0.95, 0.20, 0.30]).

% 6. INTENT EVIDENCE
% -----------------------------------------------------------------------------
intent_fact(power_delta, irs_code_section, taxpayer_subject, 0.95).
intent_fact(beneficiary, federal_treasury, revenue_stability).
