% =============================================================================
% DR MODAL LOGIC AUDIT: MINNEAPOLIS FEDERAL SURGE
% FILENAME: nyt_the_morning_2026_01_14_data.pl
% UPDATED: January 15, 2026
% =============================================================================

% -----------------------------------------------------------------------------
% Section 1: Entities & Intervals
% -----------------------------------------------------------------------------

% Structural and Organizational Entities
entity(federal_surge, structural). % Approximately 2,800 to 3,000 agents deployed
entity(somali_protections, structural). % Somali community protections for 2,000 migrants
entity(mpls_municipal_trust, structural). % Police-community relations and municipal safety duties
entity(city_stability, structural). % Load-bearing state strained by lockdowns and closures
entity(federal_administration, organizational). % Executive branch/Trump administration
entity(federal_prosecutors, organizational). % DOJ officials/Justice Department
entity(mpls_police_dept, organizational). % Chief Brian O'Hara
entity(legal_injunction_scaffold, scaffold). % The joint lawsuit and TRO mechanism

% Define Time Interval (Integers Only)
% 0: Baseline | 5: Renee Good Shooting | 15: Current Surge (January 15, 2026)
interval(surge_escalation, 0, 15).

% -----------------------------------------------------------------------------
% Section 2: Events
% -----------------------------------------------------------------------------

event(e1, killing_of_renee_good, 5, [victim(renee_good), date('2026-01-07')]). % Shot by ICE agent Jan 7
event(e2, second_shooting_incident, 14, [victim(unnamed_man), date('2026-01-14')]). % Second federal shooting Jan 14
event(e3, state_lawsuit_filed, 12, [actors([keith_ellison, jacob_frey]), date('2026-01-12')]). % Lawsuit filed Jan 12
event(e4, insurrection_act_threat, 15, [actor(president_trump), date('2026-01-15')]). % Trump threatens Insurrection Act Jan 15

% -----------------------------------------------------------------------------
% Section 3: Constraint Claims & Metrics (Current State T=15)
% -----------------------------------------------------------------------------

% Federal Surge: Claimed as a Rope (enforcement); measured as a Snare (occupation)
% 2,800 federal agents vs 600 local police creates massive tactical extractiveness
constraint_claim(federal_surge, rope).
constraint_metric(federal_surge, extractiveness, 0.88). 
constraint_metric(federal_surge, suppression_requirement, 0.92). 
constraint_metric(federal_surge, snap_back_potential, 0.85).

% Trust: Claimed as a Rope to permit recommendation-based reform
% Revenues down 50-80% as customers avoid streets due to fear
constraint_claim(mpls_municipal_trust, rope).
constraint_metric(mpls_municipal_trust, extractiveness, 0.05).
constraint_metric(mpls_municipal_trust, suppression_requirement, 0.05).

% -----------------------------------------------------------------------------
% Section 4: Temporal Measurements (Evolution History)
% -----------------------------------------------------------------------------

% Evolution of Federal Surge: Transformation rope -> snare
measurement(m1, federal_surge, extractiveness, 0, 0.15).
measurement(m2, federal_surge, extractiveness, 5, 0.45). % Post-shooting escalation
measurement(m3, federal_surge, extractiveness, 15, 0.88). % Siege state as agents storm Twin Cities

% Paired suppression metrics to detect occupation-level surge
measurement(m4, federal_surge, suppression_requirement, 0, 0.10).
measurement(m5, federal_surge, suppression_requirement, 5, 0.50). 
measurement(m6, federal_surge, suppression_requirement, 15, 0.92). % 3,000 agents deployed

% -----------------------------------------------------------------------------
% Section 5: Dependencies (Counterfactual Analysis)
% -----------------------------------------------------------------------------

% High-load dependency: Surge impacts trust; trust impacts city stability
% Local police diverted to "clean up the chaos" caused by federal agents
affects_constraint(federal_surge, mpls_municipal_trust).
affects_constraint(mpls_municipal_trust, city_stability).

% -----------------------------------------------------------------------------
% Section 6: Intent Evidence (REPAIRED LOGIC)
% -----------------------------------------------------------------------------

intent_beneficiary_class(surge_escalation, federal_executive_base). 
intent_power_change(surge_escalation, federal_executive_base, 0.75).

% CRITICAL: Every rejection MUST have a prior viability entry with exact text
intent_viable_alternative(surge_escalation, federal_prosecutors, 'DOJ investigation of ICE agent'). 
intent_alternative_rejected(surge_escalation, federal_administration, 'DOJ investigation of ICE agent'). %

% Trust-based alternatives rejected by administration
intent_viable_alternative(surge_escalation, mpls_police_dept, 'Trust-based community rebuilding').
intent_alternative_rejected(surge_escalation, federal_administration, 'Trust-based community rebuilding'). %

% -----------------------------------------------------------------------------
% Section 7: Recommendations
% -----------------------------------------------------------------------------

% AG Ellison's lawsuit as the primary mechanism for relief
recommendation(rec_1, 'Minnesota and Cities obtain judicial block of surge').
affects_constraint(rec_1, federal_surge).

veto_actor(president_trump).
veto_exposed(president_trump, rec_1). %

% -----------------------------------------------------------------------------
% Section 8: Omega Variables
% -----------------------------------------------------------------------------

% Legal outcome of the Temporary Restraining Order (TRO) is currently pending
omega_variable(om_1, conceptual, 'Judicial timeline for federal response by Jan 19').
